/*
 * scala-parser-combinators-completion
 * Copyright (c) by Nexthink S.A.
 * Lausanne, Switzerland (http://www.nexthink.com)
 * Author: jonas.chapuis@nexthink.com
 */

package com.nexthink.utils.parsing.combinator.completion

import org.junit.{Assert, Test}

import scala.util.parsing.combinator.Parsers

class ExpandedCompletionsTest {

  object FiniteArithmeticParser extends Parsers with RegexCompletionSupportWithExpansion {
    val number                   = "[0-9]+".r %> "99"
    def expr(maxNesting: Int)                = term(maxNesting) ~ ("+" | "-") ~! term(maxNesting)
    def term(maxNesting: Int)                = factor(maxNesting) ~ ("*" | "/") ~! factor(maxNesting)
    def factor(maxNesting: Int): Parser[Any] = if (maxNesting == 0) number else number | "(" ~> expr(maxNesting - 1) <~ ")"

    def exprWithExpandedCompletions() = expandedCompletions(expr(1))
  }

  @Test
  def finiteExpressionParses(): Unit = {
    val result = FiniteArithmeticParser.parse(FiniteArithmeticParser.exprWithExpandedCompletions(), "( 99 / 99 - 99 / 99 ) / ( 99 / 99 + 99 / 99 ) - ( 99 * 99 - 99 * 99 ) / ( 99 * 99 - 99 / 99 )")
    println(result)
    assert(result.successful)
  }

  @Test
  def finiteExpressionCompletesToAllAlternatives(): Unit = {
    val completions = FiniteArithmeticParser.completeString(FiniteArithmeticParser.exprWithExpandedCompletions(), "")
    println(completions)
  }

  object InfiniteExpressionParser extends Parsers with RegexCompletionSupportWithExpansion {
    val fox                      = "the quick brown fox"
    val jumpsOver                = "which jumps over the lazy"
    val jumpsOverDogOrCat        = jumpsOver ~ ("dog" | "cat")
    lazy val parser              = jumpsOverDogOrCat | jumpsOverDogOrCat ~ which()
    def which(): Parser[Any]     = expandedCompletions(parser, stop = jumpsOverDogOrCat ~ jumpsOverDogOrCat)
    lazy val infiniteDogsAndCats = fox ~ which
  }

  @Test
  def infiniteExpressionParses(): Unit = {
    val result = InfiniteExpressionParser.parse(InfiniteExpressionParser.infiniteDogsAndCats,
                                  "the quick brown fox which jumps over the lazy cat which jumps over the lazy dog which jumps over the lazy cat")
    assert(result.successful)
  }

  @Test
  def infiniteExpressionCompletesToAlternativesUpToStop(): Unit = {
    val completions = InfiniteExpressionParser.completeString(InfiniteExpressionParser.infiniteDogsAndCats, "the quick brown fox ")
    Assert.assertEquals(
      Seq(
        " which jumps over the lazy cat which jumps over the lazy cat",
        " which jumps over the lazy cat which jumps over the lazy dog",
        " which jumps over the lazy dog which jumps over the lazy cat",
        " which jumps over the lazy dog which jumps over the lazy dog"
      ),
      completions
    )
  }
}
