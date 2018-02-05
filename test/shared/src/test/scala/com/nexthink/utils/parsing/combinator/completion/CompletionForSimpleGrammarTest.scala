package com.nexthink.utils.parsing.combinator.completion

import org.scalatest.{Matchers, FlatSpec}
import monix.execution.Scheduler.Implicits.global
import CompletionTestDefinitions._

class CompletionForSimpleGrammarTest extends FlatSpec with Matchers {

  object SimpleGrammar extends CompletionTestAsserters with RegexCompletionSupport {
    val number = "[0-9]+".r %> ("1", "10", "99") % "number" %? "any number"

    def expr = term | "(" ~> term <~ ")"
    def term = number ^^ {
      _.toInt
    }
  }

  object AsyncSimpleGrammar extends CompletionTestAsserters with AsyncRegexCompletionSupport {
    val number = "[0-9]+".r %> ("1", "10", "99") % "number" %? "any number"

    def expr = term | "(" ~> term <~ ")"
    def term = number ^^ {
      _.toInt
    }
  }

  def assertSyncCompletions(in: String, expected: Set[AssertionSet]) =
    SimpleGrammar.assertHasCompletions(expected, SimpleGrammar.complete(SimpleGrammar.expr, in))
  def assertAsyncCompletions(in: String, expected: Set[AssertionSet]) =
    AsyncSimpleGrammar.assertHasCompletions(expected, AsyncSimpleGrammar.complete(AsyncSimpleGrammar.expr, in))
  def assertCompletions(in: String, expected: Set[AssertionSet]): Unit = {
    assertSyncCompletions(in, expected)
    assertAsyncCompletions(in, expected)
  }

  "empty" should "complete to number or paren" in {
    val expected = Set(Tagged("number", Some("any number"), 0, "1", "10", "99"), Default("("))
    assertCompletions("", expected)
  }

  "invalid" should "complete to nothing" in {
    assertCompletions("invalid", Set())
  }

  "left paren" should "complete to number" in {
    val expected: Set[AssertionSet] = Set(Tagged("number", Some("any number"), 0, "1", "10", "99"))
    assertCompletions("(", expected)
  }

  "left paren and number" should "complete to right parent" in {
    val expected: Set[AssertionSet] = Set(Default(")"))
    assertCompletions("(8", expected)
  }

  "left paren and invalid" should "complete to nothing" in {
    assertCompletions("(invalid", Set())
  }

  "parent number" should "complete to empty" in {
    assertCompletions("(56) ", Set())
  }

  "number" should "complete to empty" in {
    assertCompletions("(56) ", Set())
  }

}
