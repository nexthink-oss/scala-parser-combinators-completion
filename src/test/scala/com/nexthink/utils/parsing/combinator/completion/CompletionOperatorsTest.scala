/*                                                      *\
**  scala-parser-combinators completion extensions      **
**  Copyright (c) by Nexthink S.A.                      **
**  Lausanne, Switzerland (http://www.nexthink.com)     **
\*                                                      */

package com.nexthink.utils.parsing.combinator.completion
import org.json4s.JsonAST.JValue
import org.json4s.JsonDSL._
import org.junit.{Assert, Test}

import scala.util.parsing.combinator.Parsers

class CompletionOperatorsTest {

  object TestParser extends Parsers with RegexCompletionSupport {
    val someParser: Parser[String] = "parser"
  }

  @Test
  def completionSpecifiedWithBuilderIsCorrect(): Unit = {
    // Arrange
    val completions: Seq[Seq[Char]] = Seq("a", "b", "c")
    val score                       = 10
    val description                 = "some description"
    val tag                         = "some tag"
    val meta: JValue                = ("some foo" -> "some bar")

    assertCompletionsMatch(TestParser.someParser %> (completions: _*) % (tag, score) %? description %% meta,
                           completions,
                           Some(tag),
                           Some(score),
                           Some(description),
                           Some(meta))

    assertCompletionsMatch(TestParser.someParser %> (completions: _*) % (tag, score, description), completions, Some(tag), Some(score), Some(description), None)

    assertCompletionsMatch(TestParser.someParser %> (completions: _*) % (tag, score, description, meta),
                           completions,
                           Some(tag),
                           Some(score),
                           Some(description),
                           Some(meta))

    assertCompletionsMatch(
      TestParser.someParser %> (completions: _*) % TestParser.CompletionTag(tag, score, Some(description), Some(meta)),
      completions,
      Some(tag),
      Some(score),
      Some(description),
      Some(meta)
    )

    assertCompletionsMatch(TestParser.someParser %> (completions: _*) % tag %? description % score %% meta,
                           completions,
                           Some(tag),
                           Some(score),
                           Some(description),
                           Some(meta))

    assertCompletionsMatch(TestParser.someParser %> (completions: _*) % tag % score %? description %% meta,
                           completions,
                           Some(tag),
                           Some(score),
                           Some(description),
                           Some(meta))
  }

  def assertCompletionsMatch[T](sut: TestParser.Parser[T],
                                completions: Seq[Seq[Char]],
                                tag: Option[String],
                                score: Option[Int],
                                description: Option[String],
                                meta: Option[JValue]): Unit = {
    // Act
    val result = TestParser.complete(sut, "")

    // Assert
    val completionSet: TestParser.CompletionSet =
      tag.flatMap(n => result.setWithTag(n)).orElse(result.defaultSet).get
    Assert.assertEquals(tag.getOrElse(""), completionSet.tag.label)
    Assert.assertEquals(score.getOrElse(0), completionSet.tag.score)
    Assert.assertEquals(description, completionSet.tag.description)
    Assert.assertEquals(meta, completionSet.tag.meta)
    Assert.assertTrue(completions == completionSet.sortedEntries.map(_.value))
  }

  @Test
  def topCompletionsLimitsCompletionsAccordingToScore(): Unit = {
    // Arrange
    val meta: JValue                = ("some foo" -> "some bar")
    val completions = Seq("one", "two", "three", "four").zipWithIndex.map {
      case (c, s) => TestParser.Completion(c, s)
    }
    val sut = (TestParser.someParser %> TestParser.CompletionSet(completions) %%% meta).topCompletions(2)

    // Act
    val result = TestParser.complete(sut, "")

    // Assert
    Assert.assertArrayEquals(Seq("four", "three").toArray[AnyRef], result.completionStrings.toArray[AnyRef])
    Assert.assertEquals(Some(meta), result.meta)
  }
}
