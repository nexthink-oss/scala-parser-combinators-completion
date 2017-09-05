/*                                                      *\
**  scala-parser-combinators completion extensions      **
**  Copyright (c) by Nexthink S.A.                      **
**  Lausanne, Switzerland (http://www.nexthink.com)     **
\*                                                      */

package com.nexthink.utils.parsing.combinator.completion

import org.junit.Assert._
import org.junit.Test

import scala.util.parsing.combinator.RegexParsers

class CompletionForIntoTest {
  val animal = "animal"
  val machine = "machine"
  val bear = "bear"
  val lion = "lion"

  object TestParser extends RegexParsers with RegexCompletionSupport {
    val animalParser  = bear | lion
    val machineParser = "plane" | "car"
    val test          = (animal | machine) >> { meta: String => if (meta == animal) animalParser else machineParser }
  }

  @Test
  def intoParserWithoutSuccessCompletesToParser(): Unit = {
    val completions = TestParser.completeString(TestParser.test, "")
    assertEquals(Seq(animal, machine), completions)
  }

  @Test
  def intoParserWithSuccessCompletesResultingParser(): Unit = {
    val completions = TestParser.completeString(TestParser.test, animal)
    assertEquals(Seq(bear, lion), completions)
  }
}
