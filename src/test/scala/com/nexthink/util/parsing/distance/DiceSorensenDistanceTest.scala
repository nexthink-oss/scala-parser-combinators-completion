/*
 * scala-parser-combinators-completion
 * Copyright (c) by Nexthink S.A.
 * Lausanne, Switzerland (http://www.nexthink.com)
 * Author: jonas.chapuis@nexthink.com
 */

package com.nexthink.util.parsing.distance

import com.nexthink.util.parsing.distance.DiceSorensenDistance._

import org.junit.runner.RunWith
import org.scalacheck.Gen
import org.scalactic.Equality
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.PropertyChecks

@RunWith(classOf[JUnitRunner])
class DiceSorensenDistanceTest extends PropSpec with PropertyChecks with Matchers  {

  implicit val doubleEq = new Equality[Double] {
    override def areEqual(a: Double, b: Any): Boolean =
      a === b.asInstanceOf[Double] +- 0.001
  }

  property("correct score for concrete examples") {
    assert(diceSorensenSimilarity("night", "nacht") === 0.5)
    assert(diceSorensenSimilarity("GGGG", "GG") === 0.75)
    assert(diceSorensenSimilarity("all together", "altogether") === 0.833)
    assert(diceSorensenSimilarity("chrome", "google chrome") === 0.666)
    assert(diceSorensenSimilarity("jones", "johnson") === 0.428)
    assert(diceSorensenSimilarity("cord", "chrom") === 0.181)
    assert(diceSorensenSimilarity("c", "chrome") === 0.222)
    assert(diceSorensenSimilarity("c", "a") === 0.0)
    assert(diceSorensenSimilarity("c", "ac") === 0.4)
  }

  property("score is 1 for equal strings") {
    forAll() { string: String =>
      diceSorensenSimilarity(string, string) === 1.0
    }
  }

  property("symmetry") {
    forAll() { (a: String, b: String) =>
      diceSorensenSimilarity(a, b) === diceSorensenSimilarity(b, a)
    }
  }

  property("score is 0 when comparing to empty string") {
    forAll(Gen.alphaNumStr.suchThat(s => s.nonEmpty)) { nonEmptyString: String =>
      diceSorensenSimilarity(nonEmptyString, "") === 0.0
      diceSorensenSimilarity("", nonEmptyString) === 0.0
    }
  }
}
