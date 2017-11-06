package com.nexthink.utils.parsing.distance

import java.util

import scala.collection.JavaConverters._

object DiceSorensenDistance {
  def diceSorensenSimilarity(a: String, b: String): Double = {
    val aWords            = tokenizeWords(a.toLowerCase)
    val bWords            = tokenizeWords(b.toLowerCase)
    val aBigrams          = aWords.flatMap(bigramsWithAffixing).toList
    val bBigrams          = bWords.flatMap(bigramsWithAffixing).toList
    val matchesSearchList = new util.LinkedList[String](bBigrams.asJava)

    def hasMatchingBigramInB(bigram: String) = {
      def findAndRemoveBigramIter(bigram: String, iterator: util.Iterator[String]): Boolean = {
        if (!iterator.hasNext) {
          false
        } else if (iterator.next() == bigram) {
          iterator.remove()
          true
        } else {
          findAndRemoveBigramIter(bigram, iterator)
        }
      }
      findAndRemoveBigramIter(bigram, matchesSearchList.iterator())
    }

    val intersection = aBigrams.count(hasMatchingBigramInB)
    2.0 * intersection / (aBigrams.size + bBigrams.size)
  }

}
