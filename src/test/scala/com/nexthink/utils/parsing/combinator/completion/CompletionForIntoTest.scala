package com.nexthink.utils.parsing.combinator.completion

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import scala.util.parsing.combinator.RegexParsers

class CompletionForIntoTest extends FlatSpec with Matchers {
  import monix.execution.Scheduler.Implicits.global
  val animal  = "animal"
  val machine = "machine"
  val bear    = "bear"
  val lion    = "lion"

  object TestParser extends RegexParsers with RegexCompletionSupport {
    val animalParser  = bear | lion
    val machineParser = "plane" | "car"
    val test = (animal | machine) >> { meta: String =>
      if (meta == animal) animalParser else machineParser
    }
  }

  object AsyncTestParser extends RegexParsers with AsyncRegexCompletionSupport {
    val animalParser  = bear | lion
    val machineParser = "plane" | "car"
    val test = (animal | machine) >> { meta: String =>
      if (meta == animal) animalParser else machineParser
    }
  }

  def syncParserCompletesTo(in: String, completions: Seq[String]) =
    TestParser.completeString(TestParser.test, in) shouldBe completions
  def asyncParserCompletesTo(in: String, completions: Seq[String]) =
    AsyncTestParser.completeString(AsyncTestParser.test, in) shouldBe completions
  def parsersCompleteTo(in: String, completions: Seq[String]) = {
    syncParserCompletesTo(in, completions)
    asyncParserCompletesTo(in, completions)
  }

  "intoParser without success" should "complete to parser" in {
    parsersCompleteTo("", Seq(animal, machine))
  }

  "async intoParser without success" should "complete to parser" in {
    parsersCompleteTo("", Seq(animal, machine))
  }

  "intoParser with success" should "complete resulting parser" in {
    parsersCompleteTo(animal, Seq(bear, lion))
  }

  "async intoParser with success" should "complete resulting parser" in {
    parsersCompleteTo(animal, Seq(bear, lion))
  }

}
