package com.nexthink.utils.parsing.combinator.completion
import com.nexthink.utils.parsing.combinator.completion.CompletionTestDefinitions.{AssertionSet, Tagged}
import org.json4s.JsonAST.JValue
import org.json4s.JsonDSL._
import org.scalatest.{FlatSpec, Matchers}
import monix.execution.Scheduler.Implicits.global

import scala.util.parsing.combinator.Parsers

class CompletionOperatorsTest extends FlatSpec with Matchers with Parsers with AsyncRegexCompletionSupport with CompletionTestAsserters {

  val completions: Seq[String] = Seq("a", "b", "c")
  val score                    = 10
  val description              = "some description"
  val tag                      = "some tag"
  val meta: JValue             = ("some foo" -> "some bar")
  val completionMeta: JValue   = ("me" -> "ta")

  val someParser: Parser[String]           = literal("parser") %> CompletionSet(CompletionTag(tag), completions.map(Completion(_)))
  val someAsyncParser: AsyncParser[String] = "parser" %> CompletionSet(CompletionTag(tag), completions.map(Completion(_)))

  def assertSyncCompletions(p: Parser[String], in: String, expected: AssertionSet) =
    assertHasCompletions(Set(expected), complete(p, in))
  def assertAsyncCompletions(p: AsyncParser[String], in: String, expected: AssertionSet) =
    assertHasCompletions(Set(expected), complete(p, in))
  def assertCompletions(p: Parser[String], ap: AsyncParser[String], in: String, expected: AssertionSet): Unit = {
    assertSyncCompletions(p, in, expected)
    assertAsyncCompletions(ap, in, expected)
  }

  "Completions specified with builder" should "be correct" in {
    val expected = Tagged(tag, Some(description), score, Some(meta), completions, completions.map(_ => Some(completionMeta)))
    assertCompletions(
      someParser      % (tag, score) %? description %% meta %-% completionMeta,
      someAsyncParser % (tag, score) %? description %% meta %-% completionMeta,
      "",
      expected
    )
    assertCompletions(someParser      % (tag, score, description) %% meta %-% completionMeta,
                      someAsyncParser % (tag, score, description) %% meta %-% completionMeta,
                      "",
                      expected)
    assertCompletions(someParser      % (tag, score, description, meta) %-% completionMeta,
                      someAsyncParser % (tag, score, description, meta) %-% completionMeta,
                      "",
                      expected)
    assertCompletions(
      someParser      % CompletionTag(tag, score, Some(description), Some(meta)) %-% completionMeta,
      someAsyncParser % CompletionTag(tag, score, Some(description), Some(meta)) %-% completionMeta,
      "",
      expected
    )
    assertCompletions(someParser      % tag %? description % score %% meta %-% completionMeta,
                      someAsyncParser % tag %? description % score %% meta %-% completionMeta,
                      "",
                      expected)
  }

  "Top completions" should "limit completions according to score" in {
    def test[T, P[+R] <: CombinableParser[R, P]](parser: P[T])(complete: (P[T], String) => Completions): Unit = {
      // Arrange
      val meta: JValue = ("some foo" -> "some bar")
      val completions = Seq("one", "two", "three", "four").zipWithIndex.map {
        case (c, s) => Completion(c, s)
      }
      val sut = (parser %> CompletionSet(completions) %%% meta).topCompletions(2)

      // Act
      val result = complete(sut, "")

      // Assert
      Seq("four", "three").toArray[AnyRef] shouldBe result.completionStrings.toArray[AnyRef]
      Some(meta) shouldBe result.meta
    }

    test(someParser)((p: Parser[String], s: String) => complete(p, s))
    test(someAsyncParser)((p: AsyncParser[String], s: String) => complete(p, s))
  }

  "map" should "map completions correctly" in {
    def mapCompletions(c: Completions): Completions = c.map(_.map(c => c.copy(value = c.value.length.toString)))
    val expected                                    = Tagged(tag, "1", "1", "1")
    assertCompletions(someParser.map(identity, mapCompletions), someAsyncParser.map(identity, mapCompletions), "", expected)
    assertCompletions(someParser.mapCompletions(mapCompletions), someAsyncParser.mapCompletions(mapCompletions), "", expected)
  }

  "log" should "not affect parser" in {
    val expected = Tagged(tag, completions: _*)
    assertCompletions(log(someParser)("some parser"), log(someAsyncParser)("some async parser"), " ", expected)
  }

}
