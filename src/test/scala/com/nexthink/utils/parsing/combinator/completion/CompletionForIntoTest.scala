package com.nexthink.utils.parsing.combinator.completion

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import scala.util.parsing.combinator.RegexParsers

class CompletionForIntoTest extends FlatSpec with Matchers {
  val animal = "animal"
  val machine = "machine"
  val bear = "bear"
  val lion = "lion"

  object TestParser extends RegexParsers with RegexCompletionSupport {
    val animalParser  = bear | lion
    val machineParser = "plane" | "car"
    val test          = (animal | machine) >> { meta: String => if (meta == animal) animalParser else machineParser }
  }

  "intoParser without success" should "complete to parser" in {
    val completions = TestParser.completeString(TestParser.test, "")
    Seq(animal, machine) shouldBe completions
  }

  "intoParser with success" should "complete resulting parser" in {
    val completions = TestParser.completeString(TestParser.test, animal)
    Seq(bear, lion) shouldBe completions
  }


}
