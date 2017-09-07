/*                                                      *\
**  scala-parser-combinators completion extensions      **
**  Copyright (c) by Nexthink S.A.                      **
**  Lausanne, Switzerland (http://www.nexthink.com)     **
\*                                                      */

package com.nexthink.utils.parsing.combinator.completion
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods.{compact, render}
import org.junit.Assert._
import org.junit.Test

import scala.util.parsing.input.OffsetPosition

class CompletionTypesTest extends CompletionTypes {
  override type Elem = Char

  val setA = CompletionSet(
    CompletionTag("A", 10, Some("description"), Some(compact(render("type" -> "a-type")))),
    Set(Completion("a", 2), Completion("b", 1, Some(compact(render(("objects" -> Seq("devices")) ~ ("themes" -> Seq("some")))))))
  )
  val setB = CompletionSet(CompletionTag("B", 5), Set(Completion("c", 4), Completion("d", 3)))
  val setC = CompletionSet("C", Completion("e", 10))
  val setAPrime = CompletionSet(
    CompletionTag("A", 10, None, Some(compact(render("style" -> "highlight")))),
    Set(Completion("a", 4), Completion("b", 1, Some(compact(render(("objects" -> Seq("users", "packages")) ~ ("themes" -> Seq("other")))))), Completion("aa"))
  )

  @Test
  def completionsTakeTopWorks(): Unit = {
    // Arrange
    val compl = Completions(Seq(setA, setB, setC))

    // Act
    val lettersInOrder = Seq("a", "b", "c", "d", "e")
    val letterSets     = for (i <- 1 until lettersInOrder.length) yield lettersInOrder.take(i)
    letterSets.foreach(set => assertEquals(set, compl.takeTop(set.length).completionStrings))
  }

  @Test
  def completionsSetsScoredWithMaxCompletionWorks(): Unit = {
    // Arrange
    val compl = Completions(Seq(setA, setB, setC))

    // Act
    assertEquals(Seq("e", "c", "d", "a", "b"), compl.setsScoredWithMaxCompletion().completionStrings)
  }

  @Test
  def completionsAtSamePositionAreMerged(): Unit = {
    // Act
    val merged = Completions(Seq(setA, setB)) | Completions(Seq(setAPrime, setC))

    // Assert
    assertArrayEquals(
      merged.allSets.toArray[AnyRef],
      Seq(
        CompletionSet(
          CompletionTag("A", 10, Some("description"), Some(compact(render(("type" -> "a-type") ~ ("style" -> "highlight"))))),
          Set(
            Completion("a", 4),
            Completion("b", 1, Some(compact(render(("objects" -> Seq("devices", "users", "packages")) ~ ("themes" -> Seq("some", "other")))))),
            Completion("aa")
          )
        ),
        setB,
        setC
      ).toArray[AnyRef]
    )
  }

  @Test
  def completionsAtMostAdvancedPositionArePicked(): Unit = {
    // Arrange
    val foobar           = "foobar"
    val initialPosition  = OffsetPosition(foobar, 0)
    val advancedPosition = OffsetPosition(foobar, 1)

    // Act
    assertEquals((Completions(initialPosition, setA) | Completions(advancedPosition, setB)).allSets.head, setB)
    assertEquals((Completions(advancedPosition, setA) | Completions(initialPosition, setB)).allSets.head, setA)
  }
}
