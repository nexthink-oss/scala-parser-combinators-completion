package com.nexthink.utils.parsing.combinator.completion

import java.util

trait AlphabeticalSortingSupport extends RegexCompletionSupport {

  def alphabeticallySortedCompletions(terms: Iterable[String], maxCompletionsCount: Int): CompletionSet[Nothing] = {
    if (maxCompletionsCount == 0) {
      CompletionSet.empty
    } else {
      val arr         = Array.ofDim[String](maxCompletionsCount)
      var currentSize = 0

      for (t <- terms) {
        currentSize = addToSet(arr, currentSize, maxCompletionsCount, t)
      }

      val sortedStrings: Array[String] = util.Arrays.copyOf(arr, currentSize).sorted

      val completions = sortedStrings.zipWithIndex
        .map {
          case (t, rank) => Completion[Nothing](t, maxCompletionsCount - rank)
        }

      CompletionSet(completions)
    }
  }

  /**
    * efficiently add a string to fixed length array, as long the string doesn't already exist, and it isn't
    * lexicographically greater than the greatest existing entry
    * @return number of items in array
    */
  // scalastyle:off return
  private def addToSet(arr: Array[String], currentSize: Int, maxSize: Int, s: String): Int = {
    var i = 0
    while (i < currentSize) {
      if (arr(i) == s) {
        return currentSize
      }

      i = i + 1
    }

    if (currentSize < maxSize) {
      arr(currentSize) = s
      return currentSize + 1
    }

    // find largest existing entry
    var largest = 0
    var j       = 1
    while (j < maxSize) {
      if (arr(largest) < arr(j)) {
        largest = j
      }

      j = j + 1
    }

    // if s < largest existing, replace it in array
    if (s < arr(largest)) {
      arr(largest) = s
    }

    currentSize
  }
  // scalastyle:on return
}
