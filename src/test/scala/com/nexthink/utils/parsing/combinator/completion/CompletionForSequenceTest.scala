package com.nexthink.utils.parsing.combinator.completion

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import scala.util.parsing.combinator.Parsers

class CompletionForSequenceTest extends FlatSpec with Matchers {
  import monix.execution.Scheduler.Implicits.global
  val left = "left"
  val foo  = "foo"
  val bar  = "bar"
  val as   = "as"
  val df   = "df"

  object TestParser extends Parsers with RegexCompletionSupport {
    val sequence         = left ~> (foo | bar)
    val subSeqLeft       = foo ~ bar | foo
    val subSeqRight      = as ~ df | df ~ as
    val composedSequence = subSeqLeft ~ subSeqRight
  }

  object AsyncTestParser extends Parsers with AsyncRegexCompletionSupport {
    val sequence         = left ~> (foo | bar)
    val subSeqLeft       = foo ~ bar | foo
    val subSeqRight      = as ~ df | df ~ as
    val composedSequence = subSeqLeft ~ subSeqRight
  }

  def assertSyncSequence(in: String, expected: Seq[String])  = TestParser.completeString(TestParser.sequence, in) shouldBe expected
  def assertAsyncSequence(in: String, expected: Seq[String]) = AsyncTestParser.completeString(AsyncTestParser.sequence, in) shouldBe expected
  def assertCompletionsForSequence(in: String, expected: Seq[String]) = {
    assertSyncSequence(in, expected)
    assertAsyncSequence(in, expected)
  }
  def assertComposedSyncSequence(in: String, expected: Seq[String])  = TestParser.completeString(TestParser.composedSequence, in) shouldBe expected
  def assertComposedAsyncSequence(in: String, expected: Seq[String]) = AsyncTestParser.completeString(AsyncTestParser.composedSequence, in) shouldBe expected
  def assertCompletionsForComposedSequence(in: String, expected: Seq[String]) = {
    assertComposedSyncSequence(in, expected)
    assertComposedAsyncSequence(in, expected)
  }

  "empty" should "complete to left" in {
    assertCompletionsForSequence("", Seq(left))
  }

  "partial left" should "complete to left" in {
    assertCompletionsForSequence(left.dropRight(2), Seq(left))
  }

  "left" should "complete to right alternatives" in {
    assertCompletionsForSequence(left, Seq(bar, foo))
  }

  "left and right" should "complete to nothing" in {
    assertCompletionsForSequence(left + "  " + bar, Nil)
  }

  "empty composed" should "complete to left" in {
    assertCompletionsForComposedSequence("", Seq(foo))
  }

  "left composed" should "complete to left remaining alternative and right" in {
    assertCompletionsForComposedSequence(foo, Seq(as, bar, df))
  }

  "left composed" should "complete to correct right alternative" in {
    assertCompletionsForComposedSequence(foo + " " + as, Seq(df))
  }

}
