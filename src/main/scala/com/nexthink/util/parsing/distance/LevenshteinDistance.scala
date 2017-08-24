/*                                                       *\
**  scala-parser-combinators-completion                  **
**  Copyright (c) by Nexthink S.A.                       **
**  Lausanne, Switzerland (http://www.nexthink.com)      **
**  Author: jonas.chapuis@nexthink.com                   **
\*                                                       */

package com.nexthink.util.parsing.distance

/**
  * Levenshtein com.nexthink.util.parsing.distance is the classical string difference metric. It is the minimum number of single-character edits
  * (i.e. insertions, deletions or substitutions) required to change one word into another. It is typically implemented
  * with a dynamic programming approach.
  *
  * See https://en.wikipedia.org/wiki/Levenshtein_distance
  */
object LevenshteinDistance extends EditDistance[Char] {
  val LevenshteinWeights = EditWeights(constantWeightOfOne, constantWeightOfOne, constantWeightOfOne, 1)

  def constantWeightOfOne(a: Char, b: Char): Int = 1

  def normalizedLevenshteinDistance(a: String, b: String): Double = normalizedEditDistance(a, b, LevenshteinWeights)

  def normalizedLevenshteinSimilarity(a: String, b: String): Double = 1.0 - normalizedLevenshteinDistance(a, b)
}
