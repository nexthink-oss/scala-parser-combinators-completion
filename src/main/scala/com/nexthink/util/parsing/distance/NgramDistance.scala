/*                                                       *\
**  scala-parser-combinators-completion                  **
**  Copyright (c) by Nexthink S.A.                       **
**  Lausanne, Switzerland (http://www.nexthink.com)      **
**  Author: jonas.chapuis@nexthink.com                   **
\*                                                       */

package com.nexthink.util.parsing.distance

/**
  * N-gram edit com.nexthink.util.parsing.distance is an edit com.nexthink.util.parsing.distance metric which considers multiple characters at a time. N-gram edit
  * com.nexthink.util.parsing.distance takes the idea of Levenshtein com.nexthink.util.parsing.distance and treats each n-gram as a character. The impact of this
  * approach is that insertions and deletions which don't involve double letters are more heavily penalized using
  * n-grams than unigrams. In essence, it introduces a notion of context and favors strings with continuous streches
  * of equal characters (since it multiples the number of comparisons). It is generally used with bigrams, which
  * offer the best efficiency/performance ratio. We also refine this approach with some level of partial credit for
  * n-grams that share common characters. In addition, by using string affixing which allow the first character to
  * participate in the same number of n-grams as an intermediate character. Also, words that don't begin with the same
  * n-1 characters receive a penalty for not matching the prefix.
  *
  * See http://webdocs.cs.ualberta.ca/~kondrak/papers/spire05.pdf (N-Gram Similarity and Distance, Grzegorz Kondrak, 2005)
  * This approach is described in "Taming Text", chapter 4 "Fuzzy string matching", https://www.manning.com/books/taming-text
  */
object NgramDistance extends EditDistance[String] {
  def constantWeightOfArity(arity: Int)(a: String, b: String): Int = arity

  def normalizedNgramDistance(a: String, b: String, arity: Int): Double =
    normalizedEditDistance(
      ngramsWithAffixing(a)(arity),
      ngramsWithAffixing(b)(arity),
      EditWeights(insertion = constantWeightOfArity(arity), deletion = constantWeightOfArity(arity), substitution = ngramWeight, arity)
    )

  def normalizedBigramDistance(a: String, b: String): Double =
    normalizedNgramDistance(a, b, 2)

  def normalizedBigramSimilarity(a: String, b: String): Double =
    normalizedNgramSimilarity(a, b, 2)

  def normalizedNgramSimilarity(a: String, b: String, arity: Int): Double =
    1.0 - normalizedNgramDistance(a, b, arity)

  // partial credit for similar ngrams
  def ngramWeight(a: String, b: String): Int = {
    var counter = 0
    for (i <- 0 until a.length) {
      if (a.charAt(i) != b.charAt(i)) counter += 1
    }
    counter
  }
}
