package com.nexthink.utils.parsing.combinator.completion

import org.scalatest.{FlatSpec, Matchers}

import scala.util.parsing.combinator.syntactical.StandardTokenParsers

class CompletionForAcceptAndElemTest extends FlatSpec with Matchers {
  object TestParser extends StandardTokenParsers with CompletionSupport
  import TestParser.lexical._

  "elem" should "complete to passed completions" in {
    // Arrange
    val tokens = Set[Token](NumericLit("1"), NumericLit("2"), NumericLit("3"))
    val parser =
      TestParser.elem("test", _ => true, completions = tokens)

    // Act
    val result = parser.completions(new Scanner(""))

    // Assert
    result.allCompletions.map(_.value.head).toSet shouldBe tokens
  }

  "accept elem" should "complete to elem" in {
    // Arrange
    val elem   = NumericLit("1")
    val parser = TestParser.elem(elem)

    // Act
    val result = parser.completions(new Scanner(""))

    // Assert
    headToken(result.allCompletions) shouldBe elem
  }

  "accept elem list" should "complete to next in list" in {
    // Arrange
    val one    = NumericLit("1")
    val two    = NumericLit("2")
    val three  = NumericLit("3")
    val seq    = List(one, two, three)
    val parser = TestParser.accept(seq)

    // Act
    val result1     = parser.completions(new Scanner(""))
    val result2     = parser.completions(new Scanner("1"))
    val result3     = parser.completions(new Scanner("1 2"))
    val emptyResult = parser.completions(new Scanner("1 2 3"))

    // Assert
    one shouldBe headToken(result1.allCompletions)
    two shouldBe headToken(result2.allCompletions)
    three shouldBe headToken(result3.allCompletions)
    emptyResult.allCompletions.isEmpty shouldBe true
  }

  "accept with partial function" should "complete to passed completion" in {
    // Arrange
    case class Number(n: Int)
    val tokens = Set[Token](NumericLit("1"), NumericLit("2"), NumericLit("3"))
    val parser = TestParser.accept("number", { case NumericLit(n) => Number(n.toInt) }, tokens)

    // Act
    val result = parser.completions(new Scanner(""))

    // Assert
    result.allCompletions.map(_.value.head).toSet shouldBe tokens
  }

  private def headToken(completions: Iterable[TestParser.Completion[_]]) = completions.map(_.value).head.head
}
