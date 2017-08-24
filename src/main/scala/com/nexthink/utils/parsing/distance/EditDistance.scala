/*                                                       *\
**  scala-parser-combinators-completion                  **
**  Copyright (c) by Nexthink S.A.                       **
**  Lausanne, Switzerland (http://www.nexthink.com)      **
**  Author: jonas.chapuis@nexthink.com                   **
\*                                                       */

package com.nexthink.utils.parsing.distance

trait EditDistance[E] {
  private def min(params: Int*): Int = params.min

  type WeightComputation = (E, E) => Int
  case class EditWeights(insertion: WeightComputation,
                         deletion: WeightComputation,
                         substitution: WeightComputation,
                         maxPossibleWeight: Int)

  protected def normalizedEditDistance(a: Seq[E], b: Seq[E], weights: EditWeights): Double = {
    if (a.isEmpty || b.isEmpty) {
      if (a == b) 1 else 0
    } else {
      def initialCostsRow =
        Array.range(0, (a.length + 1) * weights.maxPossibleWeight, weights.maxPossibleWeight)

      var previousRow = initialCostsRow
      var newRow      = new Array[Int](a.length + 1)

      def computeCost(rowIdx: Int, colIdx: Int) = {
        val aEl = a(colIdx - 1)
        val bEl = b(rowIdx - 1)
        min(previousRow(colIdx) + weights.insertion(aEl, bEl),
            newRow(colIdx - 1) + weights.deletion(aEl, bEl),
            previousRow(colIdx - 1) + (if (aEl == bEl) 0 else weights.substitution(aEl, bEl)))
      }
      def computeRow(rowIdx: Int) = {
        newRow(0) = rowIdx * weights.maxPossibleWeight
        for (colIdx <- 1 to a.length) {
          newRow(colIdx) = computeCost(rowIdx, colIdx)
        }
      }
      def swapArrays() = {
        val tempRef = previousRow
        previousRow = newRow
        newRow = tempRef
      }

      for (rowIdx <- 1 to b.length) {
        computeRow(rowIdx)
        if (rowIdx < b.length) swapArrays()
      }
      newRow.last.toDouble / (a.length.max(b.length) * weights.maxPossibleWeight)
    }
  }
}
