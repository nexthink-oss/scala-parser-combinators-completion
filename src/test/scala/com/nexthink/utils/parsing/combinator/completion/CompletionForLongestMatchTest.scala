package com.nexthink.utils.parsing.combinator.completion

import org.scalatest.Matchers
import org.scalatest.FlatSpec

import scala.util.parsing.combinator.Parsers

class CompletionForLongestMatchTest extends FlatSpec with Matchers {
  import monix.execution.Scheduler.Implicits.global
  val foo = "foo"
  val bar = "bar"

  object Parsers extends Parsers with RegexCompletionSupport {
    val samePrefix                     = foo ||| foo ~ bar
    val constrainedAndOpenAlternatives = foo ~ bar ||| (".{5,}".r %> Completion("sample string longer than 5 char"))
  }

  object AsyncParsers extends Parsers with AsyncRegexCompletionSupport {
    val samePrefix                     = foo ||| foo ~ bar
    val constrainedAndOpenAlternatives = foo ~ bar ||| (".{5,}".r %> Completion("sample string longer than 5 char"))
  }

  "normally problematically ordered alternatives" should "parse correctly" in {
    Parsers.parseAll(Parsers.samePrefix, foo).successful shouldBe true
    AsyncParsers.parse(AsyncParsers.samePrefix, foo).successful shouldBe true
    Parsers.parseAll(Parsers.samePrefix, foo + bar).successful shouldBe true // would be false with |
    AsyncParsers.parse(AsyncParsers.samePrefix, foo + bar).successful shouldBe true
  }

  "empty" should "complete to alternatives" in {
    Parsers.completeString(Parsers.samePrefix, "") shouldBe Seq(foo)
    AsyncParsers.completeString(AsyncParsers.samePrefix, "") shouldBe Seq(foo)
  }

  "partial longer alternative" should "complete to longer alternative" in {
    Parsers.completeString(Parsers.samePrefix, foo) shouldBe Seq(bar)
    AsyncParsers.completeString(AsyncParsers.samePrefix, foo) shouldBe Seq(bar)
  }

  "longest parse" should "provide completion" in {
    Parsers.completeString(Parsers.constrainedAndOpenAlternatives, foo) shouldBe Seq(bar)
    AsyncParsers.completeString(AsyncParsers.constrainedAndOpenAlternatives, foo) shouldBe Seq(bar)
  }

}
