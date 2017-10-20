/*                                                       *\
**  scala-parser-combinators-completion                  **
**  Copyright (c) by Nexthink S.A.                       **
**  Lausanne, Switzerland (http://www.nexthink.com)      **
**  Author: jonas.chapuis@nexthink.com                   **
\*                                                       */

package com.nexthink.utils.parsing.combinator.completion

import java.util

import com.nexthink.utils.collections.lazyQuicksort
import com.nexthink.utils.collections.PrefixMap
import com.nexthink.utils.collections.Trie
import com.nexthink.utils.parsing.distance.DiceSorensenDistance.diceSorensenSimilarity
import com.nexthink.utils.parsing.distance.trigramsWithAffixing
import com.nexthink.utils.parsing.distance.tokenizeWords

import scala.util.parsing.combinator.RegexParsers

/**
  * This trait adds specialized parsers for dealing with large lists of terms, both in terms of parsing (using a fast trie-based lookup) and
  * completion (supporting fuzzy matching)
  */
trait TermsParsers extends RegexParsers with RegexCompletionSupport with TermsParsingHelpers {
  private val DefaultSimilarityThreshold          = 20
  private val CompletionCandidatesMultiplierRatio = 3

  /**
    * This defines a parser which parses any of the specified terms.
    * The parser performs a fast match by means of a trie data structure, initialized upon creation.
    * Completions will return all available terms below the matching trie node, in alphabetical order (if any)
    * @param terms the list of terms to build the parser for
    * @param maxCompletionsCount maximum number of completions returned by the parser
    * @return parser instance
    */
  def oneOfTerms(terms: Seq[String], maxCompletionsCount: Int): Parser[String] = {
    TermsParser(terms, maxCompletionsCount)
  }

  /**
    * This defines a parser which parses any of the specified terms, and is capable of fuzzing completion on the input.
    *
    * Parsing itself requires an exact match and is using the same trie-based technique as `oneOfTerms`.
    *
    * For fuzzy completion, terms are decomposed in their trigrams and stored in a map indexed by the corresponding
    * trigrams. This allows fast lookup of a set of completion candidates which share the same trigrams as the input.
    * These candidates are ranked by the number of shared trigrams with the input, and a subset of the highest ranked
    * candidates are kept. These candidates are then re-evaluated with a more precise (but slower) specified similarity
    * metric (Dice-Sorensen by default, see [[com.nexthink.utils.parsing.distance.DiceSorensenDistance]]).
    * The top candidates according to a specified maximum number are returned as completions.
    *
    * Note that terms are affixed so that the starting and ending two characters count more than the others in order to
    * favor completions which start or end with the same characters as the input.
    *
    * This approach is described in "Taming Text", chapter 4 "Fuzzy string matching", https://www.manning.com/books/taming-text
    * @param terms the list of terms to build the parser for
    * @param similarityMeasure the string similarity metric to be used. Any `(String, String) => Double` function can be passed in. Various implementations are provided: [[com.nexthink.utils.parsing.distance.DiceSorensenDistance]] (default), [[com.nexthink.utils.parsing.distance.JaroWinklerDistance]], [[com.nexthink.utils.parsing.distance.LevenshteinDistance]] & [[com.nexthink.utils.parsing.distance.NgramDistance]]. Metric choice depends on factors such as type of terms, performance, etc.
    * @param similarityThreshold the minimum similarity score for an entry to be considered as a completion candidate
    * @param maxCompletionsCount maximum number of completions returned by the parser
    * @return parser instance
    */
  def oneOfTermsFuzzy(terms: Seq[String],
                      maxCompletionsCount: Int,
                      similarityMeasure: (String, String) => Double = diceSorensenSimilarity,
                      similarityThreshold: Int = DefaultSimilarityThreshold): Parser[String] = {
    FuzzyParser(terms, similarityMeasure, similarityThreshold, maxCompletionsCount)
  }

  private object TermsParser {
    def apply(terms: Seq[String], maxCompletionsCount: Int): Parser[String] = {
      if (terms.isEmpty) {
        failure("empty terms")
      } else {
        val original = trimmedNonEmptyTerms(terms)
        val trie = Trie(normalizedTerms(original).zip(original).map {
          case (normalizedTerm, originalTerm) => (normalizedTerm, originalTerm)
        }: _*)
        new TermsParser(trie, maxCompletionsCount, completionsWhenInputEmpty = alphabeticalCompletions(terms, maxCompletionsCount))
      }
    }
  }

  sealed private class TermsParser(trie: Trie, maxCompletionsCount: Int, completionsWhenInputEmpty: CompletionSet) extends Parser[String] {
    override def apply(in: Input): ParseResult[String] = {
        tryParse(in) match {
        case Right(MatchingTerms(terms, _)) => Success(terms.last.term, in.drop(terms.last.column - in.pos.column))
        case Left(finalColumn) =>
          val start = handleWhiteSpace(in)
          if (finalColumn == in.source.length) {
            Failure("expected term but end of source reached", in.drop(start - in.offset))
          } else {
            Failure(s"no term found starting with ${in.source.subSequence(start, finalColumn).toString()}",  in.drop(start - in.offset))
          }
      }
    }

    override def completions(in: Input): Completions = {
      if (tryParse(in).isRight)
        Completions.empty
      else {
        val start = dropAnyWhiteSpace(in)
        if (start.atEnd) {
          Completions(start.pos, completionsWhenInputEmpty)
        } else {
          val possibleTerms = alphabeticalCompletions(findAllTermsWithPrefix(start, start.offset, trie), maxCompletionsCount)
          Completions(start.pos, possibleTerms)
        }
      }
    }

    protected def tryParse(in: Input): Either[Int, MatchingTerms] = {
      val start = dropAnyWhiteSpace(in)
      findAllMatchingTerms(start, start.offset, trie) match {
        case MatchingTerms(Seq(), finalColumn) => Left(finalColumn)
        case success => Right(success)
      }
    }
  }

  private def trimmedNonEmptyTerms(terms: Seq[String]) = terms.map(_.trim()).filter(_.nonEmpty)
  private def normalizedTerms(terms: Seq[String])      = terms.map(_.toLowerCase)

  def alphabeticalCompletions(terms: Iterable[String], maxCompletionsCount: Int): CompletionSet = {
    if (maxCompletionsCount == 0) {
      CompletionSet(Seq[Completion]())
    } else {
      val arr         = Array.ofDim[String](maxCompletionsCount)
      var currentSize = 0

      for (t <- terms) {
        currentSize = addToSet(arr, currentSize, maxCompletionsCount, t)
      }

      val sortedStrings: Array[String] = util.Arrays.copyOf(arr, currentSize).sorted

      val completions = sortedStrings.zipWithIndex
        .map {
          case (t, rank) => Completion(t, maxCompletionsCount - rank)
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

  private object FuzzyParser {
    def apply(terms: Seq[String], similarityMeasure: (String, String) => Double, similarityThreshold: Int, maxCompletionsCount: Int): Parser[String] = {
      if (terms.isEmpty) {
        failure("empty terms")
      } else {
        val originals                 = trimmedNonEmptyTerms(terms)
        val normalized                = normalizedTerms(originals)
        val completionsWhenInputEmpty = alphabeticalCompletions(originals, maxCompletionsCount)
        val trigramTermPairs =
          normalized.zip(originals).par.flatMap {
            case (normalizedTerm, originalTerm) =>
              tokenizeWords(normalizedTerm).flatMap(trigramsWithAffixing).map(trigram => trigram -> originalTerm)
          }
        val ngramMap = PrefixMap(trigramTermPairs.groupBy(_._1).mapValues(_.map(_._2).toArray).toSeq.seq: _*)
        val trie = Trie(normalized.zip(originals).map {
          case (normalizedTerm, originalTerm) => (normalizedTerm, originalTerm)
        }: _*)
        new FuzzyParser(ngramMap, trie, completionsWhenInputEmpty, similarityMeasure, similarityThreshold, maxCompletionsCount)
      }
    }
  }

  sealed private class FuzzyParser private (ngramMap: PrefixMap[Array[String]],
                                            trie: Trie,
                                            completionsWhenInputEmpty: CompletionSet,
                                            similarityMeasure: (String, String) => Double,
                                            similarityThreshold: Int,
                                            maxCompletionsCount: Int)
      extends TermsParser(trie, maxCompletionsCount, completionsWhenInputEmpty) {

    override def completions(in: Input): Completions = {
      if (tryParse(in).isRight)
        Completions.empty
      else {
        val start = dropAnyWhiteSpace(in)
        if (start.atEnd) {
          Completions(start.pos, completionsWhenInputEmpty)
        } else {
          fuzzyCompletions(start)
        }
      }
    }

    private val maxCandidatesCount: Int = maxCompletionsCount * CompletionCandidatesMultiplierRatio

    private def findAndScoreNgramMatches(ngrams: Seq[String]): Map[String, Int] = {
      def iter(ngram: String, remainingNgrams: Seq[String], termsFromPreviousIter: Set[String], acc: Map[String, Int]): Map[String, Int] = {
        def scoreTerm(term: String) =
          acc.getOrElse(term, 0) + (if (termsFromPreviousIter.contains(term)) {
                                      2 // count doubled occurrence for prevMatches which match same sequences of ngrams
                                    } else { 1 })
        val matchedTerms = ngramMap.getOrElse(ngram, Array())
        val matches      = matchedTerms.map(t => t -> scoreTerm(t))
        val newMap       = acc ++ matches
        if (remainingNgrams.nonEmpty) {
          iter(remainingNgrams.head, remainingNgrams.tail, matchedTerms.toSet, newMap)
        } else {
          newMap
        }
      }
      iter(ngrams.head, ngrams.tail, Set(), Map())
    }

    private def fuzzyCompletions(start: Input): Completions = {
      val incompleteTerm = remainder(start)
      val candidates     = findCandidateMatches(incompleteTerm)
      val rankedCompletions = lazyQuicksort(
        candidates.toStream
          .map {
            case (candidateTerm, _) =>
              (candidateTerm, math.round(similarityMeasure(incompleteTerm, candidateTerm) * 100.0).toInt)
          }
          .filter { case (_, similarity) => similarity >= similarityThreshold })(Ordering.by({
        case (term, similarity) => (-similarity, term)
      }))
        .take(maxCompletionsCount)
      if (rankedCompletions.nonEmpty) {
        Completions(start.pos, CompletionSet(rankedCompletions.map {
          case (term: String, score: Int) => Completion(term, score)
        }))
      } else {
        Completions.empty
      }
    }

    private def findCandidateMatches(incompleteTerm: String): Seq[(String, Int)] = {
      val trigrams                        = trigramsWithAffixing(incompleteTerm.toLowerCase)
      val matchingTerms: Map[String, Int] = findAndScoreNgramMatches(trigrams)
      matchingTerms.toSeq.sortBy(_._2).view.reverse.take(maxCandidatesCount)
    }
  }

}

object TermsParsers extends TermsParsers
