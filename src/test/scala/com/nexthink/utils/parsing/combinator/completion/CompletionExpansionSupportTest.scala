package com.nexthink.utils.parsing.combinator.completion

import com.nexthink.utils.parsing.combinator.completion.CompletionTestDefinitions.Tagged
import org.json4s.JsonAST.JValue
import org.json4s.JsonDSL._
import org.scalatest.{FlatSpec, Matchers}

import scala.util.parsing.combinator.Parsers

class CompletionExpansionSupportTest extends FlatSpec with Matchers {

  object FiniteArithmeticParser extends Parsers with CompletionExpansionSupport {
    val number                               = "[0-9]+".r %> "99"
    def expr(maxNesting: Int)                = term(maxNesting) ~ ("+" | "-") ~! term(maxNesting)
    def term(maxNesting: Int)                = factor(maxNesting) ~ "*" ~! factor(maxNesting)
    def factor(maxNesting: Int): Parser[Any] = if (maxNesting == 0) number else number | "(" ~> expr(maxNesting - 1) <~ ")"

    def exprWithExpandedCompletions() = expandedCompletions(expr(1))
  }

  "finite expression" should "parse" in {
    val result = FiniteArithmeticParser.parse(FiniteArithmeticParser.exprWithExpandedCompletions(),
                                              "( 99 * 99 + 99 * 99 ) * ( 99 * 99 + 99 * 99 ) + ( 99 * 99 + 99 * 99 ) * ( 99 * 99 + 99 * 99 )")
    assert(result.successful)
  }

  it should "complete to all alternatives" in {
    val completions = FiniteArithmeticParser.completeString(FiniteArithmeticParser.exprWithExpandedCompletions(), "")
    completions.length shouldBe 162
  }

  object InfiniteExpressionParser extends Parsers with CompletionExpansionSupport with CompletionTestAsserters {
    val globalMeta: JValue       = ("expansions" -> "global")
    val fox                      = "the quick brown fox"
    val jumpsOver                = "which jumps over the lazy" % "action"
    val jumpsOverDogOrCat        = jumpsOver ~ ("dog" | "cat") % "animal" %? "dogs and cats" % 10
    lazy val parser              = jumpsOverDogOrCat | jumpsOverDogOrCat ~ which()
    def which(): Parser[Any]     = expandedCompletionsWithLimiter(parser, limiter = jumpsOverDogOrCat ~ jumpsOverDogOrCat) %%% globalMeta
    lazy val infiniteDogsAndCats = fox ~ which
  }

  "infinite expression" should "parse" in {
    val result = InfiniteExpressionParser.parse(
      InfiniteExpressionParser.infiniteDogsAndCats,
      "the quick brown fox which jumps over the lazy cat which jumps over the lazy dog which jumps over the lazy cat"
    )
    assert(result.successful)
  }

  "infinite expression" should "complete to alternatives up to stop" in {
    val completions = InfiniteExpressionParser.complete(InfiniteExpressionParser.infiniteDogsAndCats, "the quick brown fox  ")
    22 shouldBe completions.position.column
    InfiniteExpressionParser.assertHasCompletions(
      Set(
        Tagged(
          "animal",
          Some("dogs and cats"),
          10,
          "which jumps over the lazy cat which jumps over the lazy cat",
          "which jumps over the lazy cat which jumps over the lazy dog",
          "which jumps over the lazy dog which jumps over the lazy cat",
          "which jumps over the lazy dog which jumps over the lazy dog"
        )),
      completions
    )
  }

  it should "expand including global meta" in {
    val completions = InfiniteExpressionParser.complete(InfiniteExpressionParser.infiniteDogsAndCats, "the quick brown fox  ")
    completions.meta shouldBe Some(InfiniteExpressionParser.globalMeta)
  }

  it should "not expand elsewhere if OnlyAtInputEnd" in {
    val completions = InfiniteExpressionParser.completeString(InfiniteExpressionParser.infiniteDogsAndCats, "the quick brown fox which jumps over the lazy")
    completions shouldBe Seq("cat", "dog")
  }
}
