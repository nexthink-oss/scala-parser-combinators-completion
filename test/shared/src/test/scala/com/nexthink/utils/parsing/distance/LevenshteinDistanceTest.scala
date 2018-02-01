package com.nexthink.utils.parsing.distance

import com.nexthink.utils.parsing.distance.LevenshteinDistance._

import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks

class LevenshteinDistanceTest extends PropSpec with PropertyChecks with Matchers {

  property("correct distances for concrete examples") {
    assert(normalizedLevenshteinDistance("tape", "hat") === 3 / 4.0)
    assert(normalizedLevenshteinDistance("example", "samples") === 3 / 7.0)
    assert(normalizedLevenshteinDistance("sturgeon", "urgently") == 6 / 8.0)
    assert(normalizedLevenshteinDistance("levenshtein", "frankenstein") == 6 / 12.0)
    assert(normalizedLevenshteinDistance("distance", "difference") == 5 / 10.0)
    assert(normalizedLevenshteinDistance("java was neat", "scala is great") == 7 / 14.0)
  }

  property("distance is 0 for equal strings") {
    forAll() { string: String =>
      normalizedLevenshteinDistance(string, string) === 0
    }
  }

  property("distance is 1 when comparing to empty string") {
    forAll(Gen.alphaNumStr.suchThat(s => s.nonEmpty)) { nonEmptyString: String =>
      normalizedLevenshteinDistance(nonEmptyString, "") === 1
      normalizedLevenshteinDistance("", nonEmptyString) === 1
    }
  }
}
