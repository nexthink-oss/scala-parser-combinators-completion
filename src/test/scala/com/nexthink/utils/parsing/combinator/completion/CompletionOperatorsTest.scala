package com.nexthink.utils.parsing.combinator.completion
import org.json4s.JsonAST.JValue
import org.json4s.JsonDSL._
import org.junit.{Assert, Test}
import org.scalatest.FlatSpec

import scala.util.parsing.combinator.Parsers

class CompletionOperatorsTest extends FlatSpec with Parsers with AsyncRegexCompletionSupport {
  val completions: Seq[Seq[Char]] = Seq("a", "b", "c")
  val score                       = 10
  val description                 = "some description"
  val tag                         = "some tag"
  val meta: JValue                = ("some foo" -> "some bar")

  val someParser: Parser[String]           = "parser"
  val someAsyncParser: AsyncParser[String] = "parser"

  "Completions specified with builder" should "be correct" in {
    syncCompletionSpecifiedWithBuilderIsCorrect(someParser)
    asyncCompletionSpecifiedWithBuilderIsCorrect(someAsyncParser)
  }

  def syncCompletionSpecifiedWithBuilderIsCorrect(parser: Parser[String]): Unit = {
    implicit def parserComplete(parser: Parser[String]): String => Completions = s => complete(parser, s)

    assertCompletionsMatch(parser %> (completions: _*) % (tag, score) %? description %% meta)
    assertCompletionsMatch(parser %> (completions: _*) % (tag, score, description))
    assertCompletionsMatch(parser %> (completions: _*) % (tag, score, description, meta))
    assertCompletionsMatch(parser %> (completions: _*) % CompletionTag(tag, score, Some(description), Some(meta)))
    assertCompletionsMatch(parser %> (completions: _*) % tag %? description % score %% meta)
    assertCompletionsMatch(parser %> (completions: _*) % tag % score %? description %% meta)
  }

  def asyncCompletionSpecifiedWithBuilderIsCorrect(parser: AsyncParser[String]): Unit = {
    implicit def parserComplete(parser: AsyncParser[String]): String => Completions = s => complete(parser, s)

    assertCompletionsMatch(parser %> (completions: _*) % (tag, score) %? description %% meta)
    assertCompletionsMatch(parser %> (completions: _*) % (tag, score, description))
    assertCompletionsMatch(parser %> (completions: _*) % (tag, score, description, meta))
    assertCompletionsMatch(parser %> (completions: _*) % CompletionTag(tag, score, Some(description), Some(meta)))
    assertCompletionsMatch(parser %> (completions: _*) % tag %? description % score %% meta)
    assertCompletionsMatch(parser %> (completions: _*) % tag % score %? description %% meta)
  }

  def assertCompletionsMatch(complete: String => Completions) =
    assertCompletionsMatch(complete, completions, Some(tag), Some(score), Some(description), Some(meta))

  def assertCompletionsMatch(complete: String => Completions,
                             completions: Seq[Seq[Char]],
                             tag: Option[String],
                             score: Option[Int],
                             description: Option[String],
                             meta: Option[JValue]): Unit = {
    // Act
    val result = complete("")

    // Assert
    val completionSet: CompletionSet =
      tag.flatMap(n => result.setWithTag(n)).orElse(result.defaultSet).get
    Assert.assertEquals(tag.getOrElse(""), completionSet.tag.label)
    Assert.assertEquals(score.getOrElse(0), completionSet.tag.score)
    Assert.assertEquals(description, completionSet.tag.description)
    Assert.assertEquals(meta, completionSet.tag.meta)
    Assert.assertTrue(completions == completionSet.sortedEntries.map(_.value))
  }

  "Top completions" should "limit completions according to score" in {
    // Arrange
    val meta: JValue = ("some foo" -> "some bar")
    val completions = Seq("one", "two", "three", "four").zipWithIndex.map {
      case (c, s) => Completion(c, s)
    }
    val sut = (someParser %> CompletionSet(completions) %%% meta).topCompletions(2)

    // Act
    val result = complete(sut, "")

    // Assert
    Assert.assertArrayEquals(Seq("four", "three").toArray[AnyRef], result.completionStrings.toArray[AnyRef])
    Assert.assertEquals(Some(meta), result.meta)
  }

  def topCompletionsLimitsCompletionsAccordingToScore(parser: ): Unit = {

  }
}
