/*                                                       *\
**  scala-parser-combinators-completion                  **
**  Copyright (c) by Nexthink S.A.                       **
**  Lausanne, Switzerland (http://www.nexthink.com)      **
**  Author: jonas.chapuis@nexthink.com                   **
\*                                                       */
package com.nexthink.utils.parsing.distance

import scala.annotation.tailrec

/**
  * The Jaro-Winkler com.nexthink.utils.parsing.distance measures the similarity between two strings.
  * This is a metric which is best suited for short strings such as person's names,
  * since it performs a comparison based on a limited window (whereas edit com.nexthink.utils.parsing.distance
  * methods compare all characters)
  *
  * See https://en.wikipedia.org/wiki/Jaro%E2%80%93Winkler_distance for the definition.
  * See http://alias-i.com/lingpipe/docs/api/com/aliasi/spell/JaroWinklerDistance.html for a detailed
  * explanation of the algorithm.
  */
object JaroWinklerDistance {
  val PrefixLengthLimit         = 4
  val PrefixBoostScoreThreshold = 0.7
  val PrefixBoostFactor         = 0.1

  def jaroWinklerSimilarity(a: String, b: String): Double = {
    val score = jaroScore(a, b)
    if (score <= PrefixBoostScoreThreshold) {
      score
    } else {
      score + PrefixBoostFactor * commonPrefixLength(a, b, PrefixLengthLimit) * (1.0 - score)
    }
  }

  private def jaroScore(a: String, b: String) = {
    if (a.length == 0) {
      if (b.length == 0) 1.0 else 0.0
    } else {
      val matchRange = 0.max(a.length.max(b.length) / 2 - 1)
      val bArray     = b.toCharArray.zipWithIndex

      @tailrec
      def findMatches(index: Int, prevMatches: Map[Int, Int]): Map[Int, Int] = {
        if (index == a.length) {
          prevMatches
        } else {
          val matchInB =
            bArray.slice(0.max(index - matchRange), index + matchRange + 1).find {
              case (otherChar, indexInB) =>
                !prevMatches.contains(indexInB) && otherChar == a.charAt(index)
            }
          val newIndex = index + 1
          matchInB match {
            case Some((_, indexInB)) =>
              findMatches(newIndex, prevMatches + (indexInB -> index))
            case None => findMatches(newIndex, prevMatches)
          }
        }
      }
      val matches      = findMatches(0, Map())
      val matchesCount = matches.size.toDouble
      if (matchesCount == 0) {
        0.0
      } else {
        val matchingCharsInA = matches.values.toSeq.sorted.map(a.charAt)
        val matchingCharsInB = matches.keys.toSeq.sorted.map(b.charAt)
        val halfTranspositionsCount =
          matchingCharsInA.zip(matchingCharsInB).count {
            case (idxA, idxB) => idxA != idxB
          }
        val transpositionsCount = halfTranspositionsCount / 2

        val aMatchRatio       = matchesCount / a.length
        val bMatchRatio       = matchesCount / b.length
        val exactMatchesRatio = (matchesCount - transpositionsCount) / matchesCount
        (aMatchRatio + bMatchRatio + exactMatchesRatio) / 3.0
      }
    }
  }

  private def commonPrefixLength(a: String, b: String, maxLength: Int) = {
    a.zip(b.toList).take(maxLength).takeWhile(Function.tupled(_ == _)).length
  }
}
