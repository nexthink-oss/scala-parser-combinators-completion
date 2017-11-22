package com.nexthink.utils.parsing.combinator.completion

import org.scalatest.{Matchers, FlatSpec}
import scala.util.parsing.combinator.Parsers

class CompletionForLiteralTest extends FlatSpec with Matchers {
  val someLiteral                = "literal"
  val otherLiteralWithSamePrefix = "litOther"
  val someLiteralPrefix          = "lit"

  object Parser extends Parsers with RegexCompletionSupport {
    val literal: Parser[String] = someLiteral

    val combination = someLiteral | otherLiteralWithSamePrefix
  }

  "prefix" should "complete to literal" in {
    val completion = Parser.complete(Parser.literal, " " + someLiteralPrefix)
    2 shouldBe completion.position.column
    Seq(someLiteral) shouldBe completion.completionStrings
  }

  "prefix combination" should "complete to both alternatives" in {
    val completion =
      Parser.completeString(Parser.combination, someLiteralPrefix)
    Seq(otherLiteralWithSamePrefix, someLiteral) shouldBe completion
  }

  "partial other" should "complete to other" in {
    val completion = Parser.completeString(Parser.combination,
                                           someLiteralPrefix + otherLiteralWithSamePrefix
                                             .stripPrefix(someLiteralPrefix)
                                             .head)
    Seq(otherLiteralWithSamePrefix) shouldBe completion
  }

  "whitespace" should "complete to literal" in {
    val completion =
      Parser.complete(Parser.literal, List.fill(2)(" ").mkString)
    3 shouldBe completion.position.column
    Seq(someLiteral) shouldBe completion.completionStrings
  }

  "empty" should "complete to literal" in {
    val completion = Parser.complete(Parser.literal, "")
    1 shouldBe completion.position.column
    Seq(someLiteral) shouldBe completion.completionStrings
  }

  "other" should "complete to nothing" in {
    Map() shouldBe
      Parser.complete(Parser.literal, otherLiteralWithSamePrefix).sets
  }

  "complete literal" should "complete to empty" in {
    Parser.complete(Parser.literal, someLiteral).sets.isEmpty shouldBe true
  }
}
