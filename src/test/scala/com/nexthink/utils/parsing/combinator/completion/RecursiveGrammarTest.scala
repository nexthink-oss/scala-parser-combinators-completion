package com.nexthink.utils.parsing.combinator.completion

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class RecursiveGrammarTest extends FlatSpec with Matchers {
  import CompletionTestDefinitions._

  class ExprParser extends RegexCompletionSupport with CompletionTestAsserters {
    val number = "[0-9]+".r %> ("1", "10", "99") % "number" %? "any number"
    lazy val expr = term ~ rep((("+" | "-") % "operators" %? "arithmetic operators" % 10) ~! term ^^ {
      case "+" ~ t => t
      case "-" ~ t => -t
    }) ^^ { case t ~ r => t + r.sum }
    lazy val multiplicationDivisionOperators = ("*" | "/") % "operators" %? "arithmetic operators" % 10
    lazy val term = factor ~ rep(multiplicationDivisionOperators ~! factor) ^^ {
      case f ~ Nil => f
      case f ~ r =>
        r.foldLeft(f) {
          case (prev, "*" ~ next) => prev * next
          case (prev, "/" ~ next) => prev / next
        }
    }
    lazy val factor: Parser[Int] = number ^^ { _.toInt } | "(" ~> expr <~ ")"
  }

  class AsyncExprParser extends AsyncRegexCompletionSupport with CompletionTestAsserters {
    val number = "[0-9]+".r %> ("1", "10", "99") % "number" %? "any number"
    lazy val expr = term ~ rep((("+" | "-") % "operators" %? "arithmetic operators" % 10) ~! term ^^ {
      case "+" ~ t => t
      case "-" ~ t => -t
    }) ^^ { case t ~ r => t + r.sum }
    lazy val multiplicationDivisionOperators = ("*" | "/") % "operators" %? "arithmetic operators" % 10
    lazy val term = factor ~ rep(multiplicationDivisionOperators ~! factor) ^^ {
      case f ~ Nil => f
      case f ~ r =>
        r.foldLeft(f) {
          case (prev, "*" ~ next) => prev * next
          case (prev, "/" ~ next) => prev / next
        }
    }
    lazy val factor: AsyncParser[Int] = number ^^ { _.toInt } | "(" ~> expr <~ ")"
  }

  val expParser      = new ExprParser
  val asyncExpParser = new AsyncExprParser
  import monix.execution.Scheduler.Implicits.global

  "A recursive arithmetic expression grammar:" should "parse expressions correctly" in {
    expressionsParseCorrectly((in) => expParser.parseAll(expParser.expr, in).get)
    expressionsParseCorrectly((in) => asyncExpParser.parse(asyncExpParser.expr, in).get)
  }

  def expressionsParseCorrectly[T](parse: CharSequence => T): Unit = {
    assert(1 + 2 + 3 == parse("1+2+3"))
    assert(2 * 3 == parse("2*3"))
    assert(10 / (3 + 2) == parse("(5+5)/(3+2)"))
    assert(5 * 2 / 2 == parse("(5*2/2)"))
    assert(3 - 4 - 5 == parse("3-4-5"))
  }

  it should "complete empty with number or parentheses" in {
    val expected: Set[AssertionSet] = Set(Tagged("number", Some("any number"), 0, "1", "10", "99"), Default("("))
    syncParserCompletesTo("", expected)
    asyncParserCompletesTo("", expected)
  }

  def syncParserCompletesTo(input:String, assertion: Set[AssertionSet]) = completesTo(expParser)(input, (in: CharSequence) => expParser.complete(expParser.expr, in), assertion)
  def asyncParserCompletesTo(input:String, assertion: Set[AssertionSet]) = completesTo(asyncExpParser)(input, (in: CharSequence) => asyncExpParser.complete(asyncExpParser.expr, in), assertion)
  def completesTo[P <: CompletionTestAsserters](parser: P)(input: String, complete: CharSequence => parser.Completions, assertion: Set[AssertionSet]) =
    parser.assertHasCompletions(assertion, complete(input))


  it should "complete number to operators" in {
    val expected: Set[AssertionSet] = Set(Tagged("operators", Some("arithmetic operators"), 10, "*", "+", "-", "/"))
    syncParserCompletesTo("2", expected)
    asyncParserCompletesTo("2", expected)
  }

 /* it should "complete number followed by operator to number or parentheses" in {
    val expected = Set(Tagged("number", Some("any number"), 0, "1", "10", "99"), Default("("))
    syncParserCompletesTo("2*", expected)
    asyncParserCompletesTo("2*", expected)
  }
*/
  /* @Test

  @Test
  def numberAndOperationCompletesToNumberOrParen(): Unit =
    ExprParser.assertHasCompletions(Set(Tagged("number", Some("any number"), 0, "1", "10", "99"), Default("(")),
                                    ExprParser.complete(ExprParser.expr, "2*"))

  @Test
  def parenCompletesToNumberAndParen(): Unit =
    ExprParser.assertHasCompletions(Set(Tagged("number", Some("any number"), 0, "1", "10", "99"), Default("(")),
                                    ExprParser.complete(ExprParser.expr, "("))

  @Test
  def recursiveParenAndNumberCompletesToOperatorsOrParen(): Unit =
    ExprParser.assertHasCompletions(
      Set(Tagged("operators", Some("arithmetic operators"), 10, "*", "+", "-", "/"), Default(")")),
      ExprParser.complete(ExprParser.expr, "(((2"))

  @Test
  def closedParentCompletesToOperators(): Unit =
    ExprParser.assertHasCompletions(Set(Tagged("operators", Some("arithmetic operators"), 10, "*", "+", "-", "/")),
                                    ExprParser.complete(ExprParser.expr, "(5*2/2)"))*/
}
