package com.nexthink.utils.collections

import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import com.nexthink.utils.collections.SortingHelpers.lazyQuicksort


class SortingHelpersTests extends PropSpec with PropertyChecks with Matchers {
  private val integersList = Gen.listOfN(100000, Gen.chooseNum(Int.MinValue, Int.MaxValue))
  private val topCount = Gen.chooseNum(0, 1000)

  property("lazyQuickSort supports large lists") {
    forAll(integersList, topCount) { (list: List[Int], top: Int) =>
      {
        val result = lazyQuicksort(list.toStream).take(top).toList
        result shouldBe sorted
      }
    }
  }
}
