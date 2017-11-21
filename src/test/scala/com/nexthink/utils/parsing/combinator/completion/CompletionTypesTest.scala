package com.nexthink.utils.parsing.combinator.completion
import org.json4s.JsonDSL._
import org.json4s.JsonAST.JValue
import org.junit.Assert._
import org.junit.Test
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import scala.util.parsing.input.OffsetPosition

class CompletionTypesTest extends FlatSpec with Matchers with CompletionTypes {
  override type Elem = Char

  val setA = CompletionSet(
    CompletionTag("A", 10, "description").withMeta("type" -> "a-type"),
    Set(Completion("a", 2, Some("meta1" -> "1")), Completion("b", 1).withMeta(("objects" -> Seq("devices")) ~ ("themes" -> Seq("some"))))
  )

  val setB = CompletionSet(CompletionTag("B", 5), Set(Completion("c", 4), Completion("d", 3)))
  val setC = CompletionSet("C", Completion("e", 10))
  val setAPrime = CompletionSet(
    CompletionTag("A", 10).withMeta("style" -> "highlight"),
    Set(
      Completion("a", 4, Some("meta2" -> "2")),
      Completion("b", 1).withMeta(("objects" -> Seq("users", "packages")) ~ ("themes" -> Seq("other"))),
      Completion("aa")
    )
  )

  "Completions" should "support takeTop()" in {
    // Arrange
    val compl = Completions(Seq(setA, setB, setC))

    // Act
    val lettersInOrder = Seq("a", "b", "c", "d", "e")
    val letterSets     = for (i <- 1 until lettersInOrder.length) yield lettersInOrder.take(i)
    letterSets.foreach(set => set shouldBe compl.takeTop(set.length).completionStrings)
  }

  it should "support setsScoredWithMaxCompletion()" in {
    // Arrange
    val compl = Completions(Seq(setA, setB, setC))

    // Act
    Seq("e", "c", "d", "a", "b") shouldBe compl.setsScoredWithMaxCompletion().completionStrings
  }

  it should "merge completions at same position" in {
    // Act
    val merged = Completions(Seq(setA, setB)).withMeta("context" -> Seq("contextA")) | Completions(Seq(setAPrime, setC))
      .withMeta("context" -> Seq("contextB"))

    // Assert
    assertArrayEquals(
      Seq(
        CompletionSet(
          CompletionTag("A", 10, "description").withMeta(("type" -> "a-type") ~ ("style" -> "highlight")),
          Set(
            Completion("a", 4, Some(("meta1" -> "1") ~ ("meta2" -> "2"): JValue)),
            Completion("b", 1).withMeta(("objects" -> Seq("devices", "users", "packages")) ~ ("themes" -> Seq("some", "other"))),
            Completion("aa")
          )
        ),
        setB,
        setC
      ).toArray[AnyRef],
      merged.allSets.toArray[AnyRef]
    )
    assertEquals(Some("context" -> Seq("contextA", "contextB"): JValue), merged.meta)
  }

  it should "pick completions at most advanced position" in {
    // Arrange
    val foobar           = "foobar"
    val initialPosition  = OffsetPosition(foobar, 0)
    val advancedPosition = OffsetPosition(foobar, 1)

    // Act
    assertEquals((Completions(initialPosition, setA) | Completions(advancedPosition, setB)).allSets.head, setB)
    assertEquals((Completions(advancedPosition, setA) | Completions(initialPosition, setB)).allSets.head, setA)
  }

}
