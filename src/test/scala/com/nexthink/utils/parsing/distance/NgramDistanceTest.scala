package com.nexthink.utils.parsing.distance

import com.nexthink.utils.parsing.distance.NgramDistance._

import org.junit.runner.RunWith
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.PropertyChecks

@RunWith(classOf[JUnitRunner])
class NgramDistanceTest extends PropSpec with PropertyChecks with Matchers {

  property("correct distances for concrete examples") {
    assert(normalizedBigramDistance("skyp", "skype") === 0.25)
    assert(normalizedBigramDistance("skyp", "skype handsfree support") === 0.8125)
    assert(normalizedBigramDistance("tape", "hat") === 0.6)
    assert(normalizedBigramDistance("cord", "chrom") === 0.5)
  }

  property("distance is 0 for equal strings") {
    forAll() { string: String =>
      normalizedBigramDistance(string, string) === 0
    }
  }

  property("distance is 1 when comparing to empty string") {
    forAll(Gen.alphaNumStr.suchThat(s => s.nonEmpty)) { nonEmptyString: String =>
      normalizedBigramDistance(nonEmptyString, "") === 1
      normalizedBigramDistance("", nonEmptyString) === 1
    }
  }
}
