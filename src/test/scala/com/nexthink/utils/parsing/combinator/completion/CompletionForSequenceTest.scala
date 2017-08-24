/*                                                      *\
**  scala-parser-combinators completion extensions      **
**  Copyright (c) by Nexthink S.A.                      **
**  Lausanne, Switzerland (http://www.nexthink.com)     **
\*                                                      */

package com.nexthink.utils.parsing.combinator.completion

import org.junit.{Assert, Test}

import scala.util.parsing.combinator.Parsers

class CompletionForSequenceTest {
  val left = "left"
  val foo = "foo"
  val bar = "bar"
  val as = "as"
  val df = "df"

  object TestParser extends Parsers with RegexCompletionSupport {
    val sequence = left ~> (foo | bar)

    val subSeqLeft = foo ~ bar | foo
    val subSeqRight = as ~ df | df ~ as
    val composedSequence = subSeqLeft ~ subSeqRight
  }

  @Test
  def emptyCompletesToLeft(): Unit =
    Assert.assertEquals(Seq(left), TestParser.completeString(TestParser.sequence, ""))

  @Test
  def partialLeftCompletesToLeft(): Unit =
    Assert.assertEquals(Seq(left), TestParser.completeString(TestParser.sequence, left.dropRight(2)))

  @Test
  def completeLeftCompletesToRightAlternatives(): Unit = {
    val completion = TestParser.complete(TestParser.sequence, left)
    Assert.assertEquals(left.length + 1, completion.position.column)
    Assert.assertEquals(Seq(bar, foo), completion.completionStrings)
  }

  @Test
  def completeLeftAndRightCompletesToNothing(): Unit =
    Assert.assertEquals(Nil, TestParser.completeString(TestParser.sequence, left + "  " + bar))


  @Test
  def emptyComposedCompletesToLeft(): Unit =
    Assert.assertEquals(Seq(foo), TestParser.completeString(TestParser.composedSequence, ""))

  @Test
  def leftComposedCompletesToLeftRemainingAlternativeAndRight(): Unit =
    Assert.assertEquals(Seq(as, bar, df), TestParser.completeString(TestParser.composedSequence, foo))

  @Test
  def completeLeftComposedCompletesToCorrectRightAlternative(): Unit =
    Assert.assertEquals(Seq(df), TestParser.completeString(TestParser.composedSequence, foo + " " + as))
}
