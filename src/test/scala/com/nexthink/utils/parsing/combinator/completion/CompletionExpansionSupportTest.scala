package com.nexthink.utils.parsing.combinator.completion

import com.nexthink.utils.parsing.combinator.completion.CompletionTestDefinitions.Tagged
import org.json4s.JsonAST.JValue
import org.junit.{Assert, Test}
import org.json4s.JsonDSL._

import scala.util.parsing.combinator.Parsers

class CompletionExpansionSupportTest {

  object FiniteArithmeticParser extends Parsers with CompletionExpansionSupport {
    val number                               = "[0-9]+".r %> "99"
    def expr(maxNesting: Int)                = term(maxNesting) ~ ("+" | "-") ~! term(maxNesting)
    def term(maxNesting: Int)                = factor(maxNesting) ~ "*" ~! factor(maxNesting)
    def factor(maxNesting: Int): Parser[Any] = if (maxNesting == 0) number else number | "(" ~> expr(maxNesting - 1) <~ ")"

    def exprWithExpandedCompletions() = expandedCompletions(expr(1))
  }

  @Test
  def finiteExpressionParses(): Unit = {
    val result = FiniteArithmeticParser.parse(FiniteArithmeticParser.exprWithExpandedCompletions(),
                                              "( 99 * 99 + 99 * 99 ) * ( 99 * 99 + 99 * 99 ) + ( 99 * 99 + 99 * 99 ) * ( 99 * 99 + 99 * 99 )")
    assert(result.successful)
  }

  @Test
  def finiteExpressionCompletesToAllAlternatives(): Unit = {
    val completions = FiniteArithmeticParser.completeString(FiniteArithmeticParser.exprWithExpandedCompletions(), "")
    Assert.assertEquals(completions.length, 162)
  }

  object InfiniteExpressionParser extends Parsers with CompletionExpansionSupport with CompletionTestAsserters {
    val globalMeta: JValue = ("expansions" -> "global")
    val fox                      = "the quick brown fox"
    val jumpsOver                = "which jumps over the lazy" % "action"
    val jumpsOverDogOrCat        = jumpsOver ~ ("dog" | "cat") % "animal" %? "dogs and cats" % 10
    lazy val parser              = jumpsOverDogOrCat | jumpsOverDogOrCat ~ which()
    def which(): Parser[Any]     = expandedCompletionsWithLimiter(parser, limiter = jumpsOverDogOrCat ~ jumpsOverDogOrCat) %%% globalMeta
    lazy val infiniteDogsAndCats = fox ~ which
  }

  @Test
  def infiniteExpressionParses(): Unit = {
    val result = InfiniteExpressionParser.parse(
      InfiniteExpressionParser.infiniteDogsAndCats,
      "the quick brown fox which jumps over the lazy cat which jumps over the lazy dog which jumps over the lazy cat"
    )
    assert(result.successful)
  }

  @Test
  def infiniteExpressionCompletesToAlternativesUpToStop(): Unit = {
    val completions = InfiniteExpressionParser.complete(InfiniteExpressionParser.infiniteDogsAndCats, "the quick brown fox  ")
    Assert.assertEquals(22, completions.position.column)
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

  @Test
  def infiniteExpressionExpansionIncludesGlobalMeta(): Unit = {
    val completions = InfiniteExpressionParser.complete(InfiniteExpressionParser.infiniteDogsAndCats, "the quick brown fox  ")
    Assert.assertEquals(Some(InfiniteExpressionParser.globalMeta), completions.meta)
  }

  @Test
  def expanderWithOnlyAtInputEndDoesNotExpandElsewhere(): Unit = {
    val completions = InfiniteExpressionParser.completeString(InfiniteExpressionParser.infiniteDogsAndCats, "the quick brown fox which jumps over the lazy")
    Assert.assertEquals(Seq("cat", "dog"), completions)
  }
}
