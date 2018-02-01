package com.nexthink.utils.parsing.distance

import com.nexthink.utils.parsing.distance.JaroWinklerDistance._

import org.scalacheck.Gen
import org.scalactic.Equality
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks

class JaroWinklerDistanceTest extends PropSpec with PropertyChecks with Matchers {

  implicit val doubleEq = new Equality[Double] {
    override def areEqual(a: Double, b: Any): Boolean =
      a === b.asInstanceOf[Double] +- 0.001
  }

  property("correct score for concrete examples") {
    assert(jaroWinklerSimilarity("dixon", "dicksonx") === 0.814)
    assert(jaroWinklerSimilarity("martha", "marhta") === 0.961)
    assert(jaroWinklerSimilarity("jones", "johnson") === 0.832)
    assert(jaroWinklerSimilarity("abcvwxyz", "cabvwxyz") === 0.958)
    assert(jaroWinklerSimilarity("abcefvwxyz", "fecabvwxyz") === 0.933)
    assert(jaroWinklerSimilarity("fndr", "nexthink finder") === 0.438)
    assert(jaroWinklerSimilarity("PENNSYLVANIA", "PENNCISYLVNIA") === 0.898)
    assert(jaroWinklerSimilarity("cord", "chrom") === 0.633)
  }

  property("score is 1 for equal strings") {
    forAll() { string: String =>
      jaroWinklerSimilarity(string, string) === 1.0
    }
  }

  property("symmetry") {
    forAll() { (a: String, b: String) =>
      jaroWinklerSimilarity(a, b) === jaroWinklerSimilarity(b, a)
    }
  }

  property("score is 0 when comparing to empty string") {
    forAll(Gen.alphaNumStr.suchThat(s => s.nonEmpty)) { nonEmptyString: String =>
      jaroWinklerSimilarity(nonEmptyString, "") === 0.0
      jaroWinklerSimilarity("", nonEmptyString) === 0.0
    }
  }
}
