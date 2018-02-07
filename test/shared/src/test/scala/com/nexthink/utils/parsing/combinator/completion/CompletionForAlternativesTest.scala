package com.nexthink.utils.parsing.combinator.completion

import org.scalatest.{FlatSpec, Matchers}
import monix.execution.Scheduler.Implicits.global
import scala.util.parsing.combinator.Parsers

class CompletionForAlternativesTest extends FlatSpec with Matchers {
  val left   = "left"
  val right  = "right"
  val common = "common"

  object TestParser extends Parsers with RegexCompletionSupport {
    val alternativesWithCommonFirstParser = common ~ left | common ~! right
    val alternativesWithCommonPrefix      = (common + left) | common ~ right
  }
  object AsyncTestParser extends Parsers with AsyncRegexCompletionSupport {
    val alternativesWithCommonFirstParser = common ~ left | common ~! right
    val alternativesWithCommonPrefix      = (common + left) | common ~ right
  }

  def syncParserCompletesTo(in: String, completions: Seq[String], parser: TestParser.Parser[_, _]): Any =
    TestParser.completeString(parser, in) shouldBe completions
  def asyncParserCompletesTo(in: String, completions: Seq[String], parser: AsyncTestParser.AsyncParser[_, _]): Any =
    AsyncTestParser.completeString(parser, in) shouldBe completions
  def parsersCompleteTo(in: String, completions: Seq[String], parser: TestParser.Parser[_, _], asyncParser: AsyncTestParser.AsyncParser[_, _]) = {
    syncParserCompletesTo(in, completions, parser)
    asyncParserCompletesTo(in, completions, asyncParser)
  }

  "empty" should "complete to common" in {
    parsersCompleteTo("", Seq(common), TestParser.alternativesWithCommonFirstParser, AsyncTestParser.alternativesWithCommonFirstParser)
  }

  "common" should "complete to left and right" in {
    parsersCompleteTo(common, Seq(left, right), TestParser.alternativesWithCommonFirstParser, AsyncTestParser.alternativesWithCommonFirstParser)
  }

  "common prefix" should "complete to right since completion positions are different" in {
    parsersCompleteTo(common, Seq(right), TestParser.alternativesWithCommonPrefix, AsyncTestParser.alternativesWithCommonPrefix)
  }
}
