package com.nexthink.utils.parsing.combinator.completion

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import scala.util.parsing.combinator.RegexParsers

class CompletionForChainTest extends FlatSpec with Matchers {
  import monix.execution.Scheduler.Implicits.global

  val repeated  = "rep"
  val separator = ","
  object TestParser extends RegexParsers with RegexCompletionSupport {
    val chainlParser = literal(repeated) * (separator ^^ (_ => (a: String, b: String) => a))
    val chainrParser =
      chainr1(literal(repeated), separator ^^ (_ => (a: String, b: String) => a), (a: String, b: String) => a, "")
  }
  object AsyncTestParser extends RegexParsers with AsyncRegexCompletionSupport {
    val chainlParser = asyncLiteral(repeated) * (separator ^^ (_ => (a: String, b: String) => a))
    val chainrParser =
      chainr1(asyncLiteral(repeated), separator ^^ (_ => (a: String, b: String) => a), (a: String, b: String) => a, "")
  }

  "chainl" should "complete to parser and separator alternatively" in {
    chainTest(in => TestParser.completeString(TestParser.chainlParser, in))
    chainTest(in => AsyncTestParser.completeString(AsyncTestParser.chainlParser, in))
  }

  "chainr" should "complete to parser and separator alternatively" in {
    chainTest(in => TestParser.completeString(TestParser.chainrParser, in))
    chainTest(in => AsyncTestParser.completeString(AsyncTestParser.chainrParser, in))
  }

  private def chainTest[T](complete: String => Seq[String]) = {
    val resultRep  = complete("")
    val resultSep  = complete(repeated)
    val resultRep2 = complete(s"$repeated,")
    val resultSep2 = complete(s"$repeated,$repeated")

    // Assert
    resultRep.head shouldBe repeated
    resultSep.head shouldBe separator
    resultRep2.head shouldBe repeated
    resultSep2.head shouldBe separator
  }
}
