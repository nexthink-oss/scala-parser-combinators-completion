package com.nexthink.utils.parsing.combinator.completion

import org.scalatest.{Matchers, FlatSpec}
import monix.execution.Scheduler.Implicits.global
import scala.util.parsing.combinator.Parsers

class CompletionForRepetitionTest extends FlatSpec with Matchers {
  val repeated  = "repeated"
  val separator = "separator"
  val n         = 5

  object TestParser extends Parsers with RegexCompletionSupport {
    val repSequence    = rep(repeated)
    val repSepSequence = repsep(repeated, separator)
    val error          = repsep(repeated, err("some error"))
    val repNSequence   = repN(5, repeated)

    val subSeqLeft       = "foo" ~ "bar" | "foo"
    val subSeqRight      = "as" ~ "df" | "df" ~ "as"
    val composedSequence = subSeqLeft ~ subSeqRight
    val repAlternatives  = rep1sep("foo" | composedSequence, "and")
    val rep1Alternatives = rep1("foo" | composedSequence)
    val repNAlternatives = repN(5, "foo" | composedSequence)
  }

  object AsyncTestParser extends Parsers with AsyncRegexCompletionSupport {
    val repSequence    = rep(repeated)
    val repSepSequence = repsep(repeated, separator)
    val error          = repsep(repeated, err("some error"))
    val repNSequence   = repN(5, repeated)

    val subSeqLeft       = "foo" ~ "bar" | "foo"
    val subSeqRight      = "as" ~ "df" | "df" ~ "as"
    val composedSequence = subSeqLeft ~ subSeqRight
    val repAlternatives  = rep1sep("foo" | composedSequence, "and")
    val rep1Alternatives = rep1("foo" | composedSequence)
    val repNAlternatives = repN(5, "foo" | composedSequence)
  }

  def syncParserCompletesTo(in: String, completions: Seq[String], parser: TestParser.Parser[_]): Any =
    TestParser.completeString(parser, in) shouldBe completions
  def asyncParserCompletesTo(in: String, completions: Seq[String], parser: AsyncTestParser.AsyncParser[_]): Any =
    AsyncTestParser.completeString(parser, in) shouldBe completions
  def parsersCompleteTo(in: String, completions: Seq[String], parser: TestParser.Parser[_], asyncParser: AsyncTestParser.AsyncParser[_]) = {
    syncParserCompletesTo(in, completions, parser)
    asyncParserCompletesTo(in, completions, asyncParser)
  }

  def syncParserParses(in: String, parser: TestParser.Parser[_]): Any =
    TestParser.parse(parser, in).successful shouldBe true
  def asyncParserParses(in: String, parser: AsyncTestParser.AsyncParser[_]): Any =
    AsyncTestParser.parse(parser, in).successful shouldBe true
  def parsersParse(in: String, parser: TestParser.Parser[_], asyncParser: AsyncTestParser.AsyncParser[_]) = {
    syncParserParses(in, parser)
    asyncParserParses(in, asyncParser)
  }

  "empty rep" should "complete to repeated" in {
    parsersCompleteTo("", Seq(repeated), TestParser.repSequence, AsyncTestParser.repSequence)
  }

  "n instances and partial rep" should "complete to repeated" in {
    parsersCompleteTo(List.fill(3)(repeated).mkString + repeated.dropRight(3), Seq(repeated), TestParser.repSequence, AsyncTestParser.repSequence)
  }

  "n instances of repeated repn" should "complete to repeated" in {
    parsersCompleteTo(List.fill(3)(repeated).mkString, Seq(repeated), TestParser.repNSequence, AsyncTestParser.repNSequence)
  }

  "n instances of partial complete repn" should "complete to repeated" in {
    parsersCompleteTo(List.fill(3)(repeated).mkString + repeated.dropRight(3), Seq(repeated), TestParser.repNSequence, AsyncTestParser.repNSequence)
  }

  "n instances followed by error rep" should "complete to nothing" in {
    parsersCompleteTo(List.fill(3)(repeated).mkString + "error", Nil, TestParser.repSequence, AsyncTestParser.repSequence)
  }

  "repsep with empty" should "complete to repeated" in {
    parsersCompleteTo("", Seq(repeated), TestParser.repSepSequence, AsyncTestParser.repSepSequence)
  }

  "repeated and separator repsep" should "complete to repeated" in {
    parsersCompleteTo(repeated + separator, Seq(repeated), TestParser.repSepSequence, AsyncTestParser.repSepSequence)
  }

  "error repsep" should "complete to nothing" in {
    parsersCompleteTo(repeated, Nil, TestParser.error, AsyncTestParser.error)
  }

  "empty repn" should "complete to repeated" in {
    parsersCompleteTo("", Seq(repeated), TestParser.repNSequence, AsyncTestParser.repNSequence)
  }

  "rep alternatives" should "complete to alternatives" in {
    parsersCompleteTo("foo and foo", Seq("and", "as", "bar", "df"), TestParser.repAlternatives, AsyncTestParser.repAlternatives)
  }

  "rep1 alternatives" should "complete to alternatives" in {
    parsersCompleteTo("foo", Seq("as", "bar", "df", "foo"), TestParser.rep1Alternatives, AsyncTestParser.rep1Alternatives)
  }

  "repn alternatives" should "complete to alternatives" in {
    parsersCompleteTo("foo foo", Seq("as", "bar", "df", "foo"), TestParser.repNAlternatives, AsyncTestParser.repNAlternatives)
  }

  "repn" should "parse" in {
    parsersParse("repeated repeated repeated repeated repeated", TestParser.repNSequence, AsyncTestParser.repNSequence)
  }
}
