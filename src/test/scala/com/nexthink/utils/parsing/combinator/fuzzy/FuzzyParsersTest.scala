/*
 * scala-parser-combinators-completion
 * Copyright (c) by Nexthink S.A.
 * Lausanne, Switzerland (http://www.nexthink.com)
 * Author: jonas.chapuis@nexthink.com
 */

package com.nexthink.utils.parsing.combinator.fuzzy

import org.junit.runner.RunWith
import org.scalacheck.Gen
import org.scalatest.{Inside, Matchers, PropSpec}
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.PropertyChecks

// scalastyle:off magic.number

@RunWith(classOf[JUnitRunner])
class FuzzyParsersTest extends PropSpec with PropertyChecks with Matchers with Inside {
  object fuzzyParsers extends FuzzyParsers
  private val nonEmptyTerm                   = Gen.alphaNumStr.suchThat(t => t.nonEmpty && t.forall(_ >= ' '))
  private val nonEmptyTermLargerThanTwoChars = nonEmptyTerm.suchThat(t => t.length > 2)
  private val sampleTerms                    = Gen.containerOfN[List, String](10, nonEmptyTerm).suchThat(list => list.nonEmpty)
  private val variableLengthWhitespace       = Gen.choose(1, 10).map(i => List.fill(i)(" ").mkString)
  private val sampleTermsLargerThanTwoChars =
    Gen.containerOfN[List, String](10, nonEmptyTermLargerThanTwoChars).suchThat(list => list.nonEmpty)

  property("oneOfTerms with concrete examples") {
    val terms = fuzzyParsers.oneOfTerms(
      List("7-Zip",
           "Skype",
           "Skype Monitor",
           "Skype Handsfree Support",
           "Activity Monitor",
           "Adobe Acrobat",
           "Google Chrome",
           "GoToMeeting",
           "NEXThink Finder"))
    val samples = Table(
      "chrom" -> "Google Chrome",
      "finde" -> "NEXThink Finder",
      "fnder" -> "NEXThink Finder",
      "skyp"  -> "Skype",
      "skyp"  -> "Skype Handsfree Support"
    )
    forAll(samples) { (partial: String, term: String) =>
      val completions = fuzzyParsers.completeString(terms, partial)
      completions should contain(term)
    }
  }

  property("oneOfTerms completes to nothing if term is complete even followed with whitespace") {
    forAll(nonEmptyTermLargerThanTwoChars, variableLengthWhitespace) { (term: String, whitespace: String) =>
      val parser      = fuzzyParsers.oneOfTerms(Seq(term))
      val completions = fuzzyParsers.completeString(parser, term + whitespace)
      completions shouldBe empty
    }
  }

  property("oneOfTerms does parse all terms (symmetry)") {
    forAll(sampleTerms) { terms: List[String] =>
      {
        val parser = fuzzyParsers.oneOfTerms(terms)
        terms.foreach { term =>
          inside(fuzzyParsers.parse(parser, term)) {
            case fuzzyParsers.Success(parsedLiteral, reader) =>
              parsedLiteral === term
            case fuzzyParsers.NoSuccess(msg, _) => fail(msg)
          }
        }
      }
    }
  }

  property("oneOfTerms with empty completes with all terms in alphabetical order") {
    forAll(sampleTerms) { terms: List[String] =>
      {
        val parser      = fuzzyParsers.oneOfTerms(terms)
        val completions = fuzzyParsers.complete(parser, " ")
        withClue(s"terms=$terms, completions=$completions") {
          completions.defaultSet.isDefined shouldBe true
          terms.distinct.sorted.zipAll(completions.completionStrings, "extraCompletion", "missingCompletion").foreach {
            case (expected, actual) => actual === expected
          }
        }
      }
    }
  }

  property("oneOfTerms with same similarity completes in alphabetical order") {
    forAll(sampleTerms) { terms: List[String] =>
      {
        val parser = fuzzyParsers.oneOfTerms(terms, similarityMeasure = (_, _) => 100)
        terms.foreach(term => {
          val completions = fuzzyParsers.complete(parser, term.take(1))
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

  property("oneOfTerms completions with prefix or suffix always includes full term (thanks to affixing)") {
    forAll(sampleTermsLargerThanTwoChars) { terms: List[String] =>
      {
        val parser = fuzzyParsers.oneOfTerms(terms, similarityThreshold = 0)
        terms.foreach(term => {
          forAll(Gen.choose(1, term.length - 1), Gen.oneOf(true, false)) { (substringLength, isPrefix) =>
            val substring =
              if (isPrefix) term.take(substringLength) else term.takeRight(substringLength)
            val completions =
              fuzzyParsers.complete(parser, substring).completionStrings
            withClue(s"substring=$substring, term=$term, completions=$completions, terms=$terms") {
              completions should contain(term)
            }
          }
        })
      }
    }
  }

  property("oneOfTerms with maxCompletions limits completions count to that number") {
    forAll(sampleTerms, Gen.choose(0, 10)) { (terms: List[String], count: Int) =>
      {
        val parser      = fuzzyParsers.oneOfTerms(terms, maxCompletionsCount = count)
        val completions = fuzzyParsers.complete(parser, "").completionStrings
        completions.size should be(terms.size.min(count))
      }
    }
  }

  property("oneOfTerms with similarity threshold limits completions count to that number") {
    forAll(sampleTermsLargerThanTwoChars, Gen.choose(0, 100)) { (terms: List[String], threshold: Int) =>
      {
        val parser = fuzzyParsers.oneOfTerms(terms, similarityThreshold = threshold)
        terms.foreach(term => {
          val substring   = term.drop(1)
          val completions = fuzzyParsers.complete(parser, substring)
          withClue(s"substring=$substring, term=$term, completions=$completions, terms=$terms") {
            whenever(completions.nonEmpty) {
              completions.allSets.flatMap(_.completions).map(_.score).max should (be >= threshold or equal(0))
            }
          }
        })
      }
    }
  }

}
