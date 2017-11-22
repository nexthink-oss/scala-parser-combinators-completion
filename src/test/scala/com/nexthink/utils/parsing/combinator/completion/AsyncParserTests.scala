package com.nexthink.utils.parsing.combinator.completion

import com.nexthink.utils.parsing.combinator.completion.CompletionTestDefinitions.{Tagged, Default}
import monix.eval.Task
import monix.execution.schedulers.TestScheduler
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._
import scala.language.postfixOps

class AsyncParserTests extends FlatSpec with Matchers {
  object AsyncGrammar extends AsyncRegexCompletionSupport with CompletionTestAsserters {
    val number = "[0-9]+".r %> ("1", "10", "99") % "number" %? "any number" ^^ { _.toInt }
    val term =
      AsyncParser(in => Task.eval(number(in)).delayExecution(1 second).flatten, in => Task.eval(number.completions(in)).delayExecution(1 second).flatten)
    def expr = term | "(" ~> term <~ ")"
  }

  implicit val testScheduler = TestScheduler()

  "empty" should "complete to number or paren" in {
    var asserted = false
    val expected = Set(Tagged("number", Some("any number"), 0, "1", "10", "99"), Default("("))
    val asyncCompletion =
      AsyncGrammar
        .completeAsync(AsyncGrammar.term, "")
        .map(AsyncGrammar.assertHasCompletions(expected, _))
        .doOnFinish(_ => Task.eval({ asserted = true }))
        .runAsync

    // simulate passage of time
    testScheduler.tick(1 second)

    asserted shouldBe true
  }
}
