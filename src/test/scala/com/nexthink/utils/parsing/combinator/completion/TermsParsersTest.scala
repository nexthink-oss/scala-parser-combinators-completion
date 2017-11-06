package com.nexthink.utils.parsing.combinator.completion

import com.nexthink.utils.parsing.combinator.completion.TermsParsersTest.termsParsers$
import org.junit.runner.RunWith
import org.scalacheck.Gen
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Inside, Matchers, PropSpec}

// scalastyle:off magic.number
object TermsParsersTest {
  object termsParsers$ extends TermsParsers
}

@RunWith(classOf[JUnitRunner])
class TermsParsersTest extends PropSpec with PropertyChecks with Matchers with Inside {
  import termsParsers$._
  val maxCompletions = 15
  private val nonEmptyTerm                   = Gen.alphaNumStr.suchThat(t => t.nonEmpty && t.forall(_ >= ' '))
  private val nonEmptyTermLargerThanTwoChars = nonEmptyTerm.suchThat(t => t.length > 2)
  private val sampleTerms                    = Gen.containerOfN[List, String](10, nonEmptyTerm).suchThat(list => list.nonEmpty)
  private val variableLengthWhitespace       = Gen.choose(1, 10).map(i => List.fill(i)(" ").mkString)
  private val sampleTermsLargerThanTwoChars =
    Gen.containerOfN[List, String](10, nonEmptyTermLargerThanTwoChars).suchThat(list => list.nonEmpty)
  private val sampleUniqueTermsLargerThanTwoChars =  Gen.containerOfN[Set, String](10, nonEmptyTermLargerThanTwoChars).suchThat(list => list.nonEmpty)
  private val examples =
    List("", "7-Zip", "Skype", "Skype Monitor", "Skype Handsfree Support", "Activity Monitor", "Adobe Acrobat", "Google Chrome", "GoToMeeting", "NEXThink Finder")

  property("oneOfTerms with partial input fails") {
    val terms  = oneOfTerms(examples, maxCompletions)
    val result = parse(terms, "skyp")
    result.successful shouldBe false
    remainder(result.next) shouldBe "skyp"
    result.next.pos.column shouldBe 1
  }

  property("oneOfTerms returns correct next with partial success") {
    val terms  = oneOfTerms(examples, maxCompletions)
    val result = parse(terms, "skype hands")
    result.successful shouldBe true
    remainder(result.next) shouldBe " hands"
    result.next.pos.column shouldBe 6
  }

  property("oneOfTerms returns correct next with success") {
    val terms  = oneOfTerms(examples, maxCompletions)
    val result = parse(terms, "skype foobar")
    result.successful shouldBe true
    remainder(result.next) shouldBe " foobar"
    result.next.pos.column shouldBe 6
  }

  property("oneOfTerms completions with concrete examples") {
    val terms = oneOfTerms(examples, maxCompletions)
    val samples = Table(
      heading = ("input", "result"),
      "skyp" -> "Skype, Skype Handsfree Support, Skype Monitor",
      "sa" -> "",
      "NEXT" -> "NEXThink Finder",
      "A"    -> "Activity Monitor, Adobe Acrobat"
    )
    forAll(samples) { (partial: String, options: String) =>
      val completedTerms: Seq[String] = if (options.isEmpty) Nil else options.split(",").map(_.trim)
      val completions    = completeString(terms, partial)
      completions shouldBe completedTerms
    }
  }

  property("oneOfTerms completions with no terms returns empty") {
    val terms = oneOfTerms(examples, maxCompletions)
    val completions = complete(terms, "sa")
    completions shouldBe Completions.empty
  }

  property("oneOfTermsFuzzy completions with concrete examples") {
    val terms = oneOfTermsFuzzy(examples, maxCompletions)
    val samples = Table(
      "chrom" -> "Google Chrome",
      "finde" -> "NEXThink Finder",
      "fnder" -> "NEXThink Finder",
      "skyp"  -> "Skype",
      "skyp"  -> "Skype Handsfree Support"
    )
    forAll(samples) { (partial: String, term: String) =>
      val completions = completeString(terms, partial)
      completions should contain(term)
    }
  }

  val termsParsers = Gen.oneOf((terms: Seq[String]) => oneOfTerms(terms, maxCompletions), (terms: Seq[String]) => oneOfTermsFuzzy(terms, maxCompletions))

  property("oneOfTerms completes to nothing if term is complete even followed with whitespace") {
    forAll(nonEmptyTermLargerThanTwoChars, variableLengthWhitespace, termsParsers) { (term: String, whitespace: String, parserCreator: (Seq[String]) => Parser[String]) =>
      {
        val parser      = parserCreator(Seq(term))
        val completions = completeString(parser, term + whitespace)
        completions shouldBe empty
      }
    }
  }

  property("oneOfTerms completes to nothing if input doesn't match any term"){
    forAll(sampleUniqueTermsLargerThanTwoChars) { (terms: Set[String] ) =>
      val excludedTerm = terms.head
      val includedTerms = terms.tail.toSeq
      val parser = oneOfTerms(includedTerms, maxCompletions)
      val completions = completeString(parser, excludedTerm)
      completions shouldBe empty
    }
  }

  property("oneOfTerms parses all terms (symmetry)") {
    forAll(sampleTerms, termsParsers) { (terms: List[String], parserCreator: (Seq[String]) => Parser[String]) =>
      {
        val parser = parserCreator(terms)
        terms.foreach { term =>
          inside(parse(parser, term)) {
            case Success(parsedLiteral, reader) =>
              parsedLiteral === term
            case NoSuccess(msg, _) => fail(msg)
          }
        }
      }
    }
  }

  property("parser with empty completes with all terms in alphabetical order") {
    forAll(sampleTerms, termsParsers) { (terms: List[String], parserCreator: (Seq[String]) => Parser[String]) =>
      {
        val parser      = parserCreator(terms)
        val completions = complete(parser, " ")
        withClue(s"terms=$terms, completions=$completions") {
          completions.defaultSet.isDefined shouldBe true
          terms.distinct.sorted.zipAll(completions.completionStrings, "extraCompletion", "missingCompletion").foreach {
            case (expected, actual) => actual === expected
          }
        }
      }
    }
  }

  property("parser with empty terms fails parsing") {
    forAll(Gen.alphaNumStr, termsParsers) { (someInput: String, parserCreator: (Seq[String]) => Parser[String]) =>
      val parser = parserCreator(Seq())
      val result = parse(parser, someInput)
      result.successful shouldBe false
    }
  }

  property("parser with empty terms completes to empty") {
    forAll(Gen.alphaNumStr, termsParsers) { (someInput: String, parserCreator: (Seq[String]) => Parser[String]) =>
      val parser = parserCreator(Seq())
      val result = complete(parser, someInput)
      result shouldBe Completions.empty
    }
  }

  property("parser with empty spaces completes at the last input position") {
    forAll(sampleTerms, Gen.chooseNum(1, 10), termsParsers) { (terms: List[String], spacesCount: Int, parserCreator: (Seq[String]) => Parser[String]) =>
      {
        val spaces = Seq.range(0, spacesCount).map(_ => " ").mkString
        val parser      = parserCreator(terms)
        val completions = complete(parser, spaces)
        withClue(s"terms=$terms, completions=$completions") {
          completions.position.column shouldBe (1 + spacesCount)
        }
      }
    }
  }

  property("oneOfTermsFuzzy with same similarity completes in alphabetical order") {
    forAll(sampleTerms) { terms: List[String] =>
      {
        val parser = oneOfTermsFuzzy(terms, maxCompletions, similarityMeasure = (_, _) => 100)
        terms.foreach(term => {
          val completions = complete(parser, term.take(1))
          withClue(s"terms=$terms, completions=$completions") {
            terms.distinct.sorted
              .zipAll(completions.completionStrings, "extraCompletion", "missingCompletion")
              .foreach {
                case (expected, actual) => actual === expected
              }
          }
        })
      }
    }
  }

  property("oneOfTermsFuzzy completions with prefix or suffix always includes full term (thanks to affixing)") {
    forAll(sampleTermsLargerThanTwoChars) { terms: List[String] =>
      {
        val parser = oneOfTermsFuzzy(terms, maxCompletions, similarityThreshold = 0)
        terms.foreach(term => {
          forAll(Gen.choose(1, term.length - 1), Gen.oneOf(true, false)) { (substringLength, isPrefix) =>
            val substring =
              if (isPrefix) term.take(substringLength) else term.takeRight(substringLength)
            val completions =
              complete(parser, substring).completionStrings
            withClue(s"substring=$substring, term=$term, completions=$completions, terms=$terms") {
              completions should contain(term)
            }
          }
        })
      }
    }
  }

  property("oneOfTermsFuzzy with maxCompletions limits completions count to that number") {
    forAll(sampleTerms, Gen.choose(0, 10)) { (terms: List[String], count: Int) =>
      {
        val parser      = oneOfTermsFuzzy(terms, maxCompletionsCount = count)
        val completions = complete(parser, "").completionStrings
        completions.size should be(terms.size.min(count))
      }
    }
  }

  property("oneOfTermsFuzzy with similarity threshold limits completions count to that number") {
    forAll(sampleTermsLargerThanTwoChars, Gen.choose(0, 100)) { (terms: List[String], threshold: Int) =>
      {
        val parser = oneOfTermsFuzzy(terms, maxCompletions, similarityThreshold = threshold)
        terms.foreach(term => {
          val substring   = term.drop(1)
          val completions = complete(parser, substring)
          withClue(s"substring=$substring, term=$term, completions=$completions, terms=$terms") {
            whenever(completions.nonEmpty) {
              completions.allSets.flatMap(_.entries).map(_.score).max should (be >= threshold or equal(0))
            }
          }
        })
      }
    }
  }

}
