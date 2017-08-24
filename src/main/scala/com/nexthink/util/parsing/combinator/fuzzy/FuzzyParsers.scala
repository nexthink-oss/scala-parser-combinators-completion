/*                                                       *\
**  scala-parser-combinators-completion                  **
**  Copyright (c) by Nexthink S.A.                       **
**  Lausanne, Switzerland (http://www.nexthink.com)      **
**  Author: jonas.chapuis@nexthink.com                   **
\*                                                       */

package com.nexthink.util.parsing.combinator.fuzzy

import com.nexthink.util.parsing.distance._
import com.nexthink.util.collections.PrefixMap
import com.nexthink.util.parsing.distance.DiceSorensenDistance.diceSorensenSimilarity
import com.nexthink.util.parsing.combinator.completion.RegexCompletionSupport

import scala.util.parsing.combinator.RegexParsers

/**
  * This trait adds a parser capable of fuzzing completion on the input to match a set of terms.
  * Parsing itself requires an exact match and is really fast thanks to a prefix trie lookup on each input char.
  *
  * For fuzzy completion, terms are decomposed in their trigrams and stored in a map indexed by the corresponding
  * trigrams. This allows fast lookup of a set of completion candidates which share the same trigrams as the input.
  * These candidates are ranked by the number of shared trigrams with the input, and a subset of the highest ranked
  * candidates are kept. These candidates are then re-evaluated with a more precise (but slower) specified similarity
  * metric (bigram edit com.nexthink.util.parsing.distance by default, see [[com.nexthink.util.parsing.distance.NgramDistance]]).
  * The top candidates according to a specified maximum number are returned as completions.
  *
  * Note that terms are affixed so that the starting and ending two characters count more than the others in order to
  * favor completions which start or end with the same characters as the input.
  *
  * This approach is described in "Taming Text", chapter 4 "Fuzzy string matching", https://www.manning.com/books/taming-text
  */
trait FuzzyParsers extends RegexParsers with RegexCompletionSupport with FuzzyParsingHelpers {
  val DefaultMaxCompletionsCount                  = 15
  private val DefaultSimilarityThreshold          = 20
  private val CompletionCandidatesMultiplierRatio = 3

  def oneOfTerms(terms: Seq[String],
                 similarityMeasure: (String, String) => Double = diceSorensenSimilarity,
                 similarityThreshold: Int = DefaultSimilarityThreshold,
                 maxCompletionsCount: Int = DefaultMaxCompletionsCount): Parser[String] = {
    new FuzzyParser(terms, similarityMeasure, similarityThreshold, maxCompletionsCount)
  }

  sealed private class FuzzyParser(terms: Seq[String], similarityMeasure: (String, String) => Double, similarityThreshold: Int, maxCompletionsCount: Int)
      extends Parser[String] {
    private def originalTerms   = terms.map(_.trim()).filter(_.nonEmpty)
    private def normalizedTerms = originalTerms.map(_.toLowerCase)

    val trie: PrefixMap[String] = PrefixMap(normalizedTerms.zip(originalTerms).map {
      case (normalizedTerm, originalTerm) => (normalizedTerm, originalTerm)
    }: _*)

    val ngramMap: PrefixMap[Array[String]] = {
      val trigramTermPairs =
        normalizedTerms.zip(originalTerms).flatMap {
          case (normalizedTerm, originalTerm) =>
            tokenizeWords(normalizedTerm).flatMap(trigramsWithAffixing).map(trigram => trigram -> originalTerm)
        }
      PrefixMap(trigramTermPairs.groupBy(_._1).mapValues(_.map(_._2).toArray).toSeq: _*)
    }

    override def apply(in: Input): ParseResult[String] = {
      val start = dropWhiteSpace(in)
      val (terms, finalPosition) =
        findAllMatchingTerms(start, start.offset, trie)
      val next = in.drop(finalPosition - in.offset)
      terms.lastOption match {
        case Some(term) => Success(term, next)
        case None =>
          if (finalPosition == in.source.length) {
            Failure(s"expected term but end of source reached", next)
          } else {
            Failure(s"no term found starting with ${subSequence(start, finalPosition)}", next)
          }
      }
    }

    override def completions(in: Input): Completions = {
      apply(in) match {
        case Success(_, _) => Completions.empty
        case NoSuccess(_, _) =>
          val start = dropWhiteSpace(in)
          if (start.atEnd) {
            // return everything
            alphabeticalCompletions(start)
          } else {
            fuzzyCompletions(start)
          }
      }
    }

    val maxCandidatesCount: Int = maxCompletionsCount * CompletionCandidatesMultiplierRatio

    private def alphabeticalCompletions(in: Input): Completions = {
      val matches = ngramMap
        .withPrefix(remainder(in).toLowerCase)
        .values
        .flatten
        .toList
        .distinct
        .sorted
        .take(maxCompletionsCount)
      if (matches.nonEmpty) {
        Completions(in.pos, CompletionSet(matches.reverse.zipWithIndex.map {
          case (t, rank) => Completion(t, rank)
        }.toSet))
      } else {
        Completions.empty
      }
    }

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

    private def fuzzyCompletions(in: Input): Completions = {
      val incompleteTerm = remainder(in)
      val candidates     = findCandidateMatches(incompleteTerm)
      val rankedCompletions = candidates
        .map {
          case (candidateTerm, _) =>
            (candidateTerm, math.round(similarityMeasure(incompleteTerm, candidateTerm) * 100.0).toInt)
        }
        .filter { case (_, similarity) => similarity >= similarityThreshold }
        .sortBy {
          case (term, similarity) => (-similarity, term)
        }
        .take(maxCompletionsCount)
      if (rankedCompletions.nonEmpty) {
        Completions(in.pos, CompletionSet(rankedCompletions.map {
          case (term: String, score: Int) => Completion(term, score)
        }))
      } else {
        Completions.empty
      }
    }

    private def findCandidateMatches(incompleteTerm: String): Seq[(String, Int)] = {
      val trigrams                        = trigramsWithAffixing(incompleteTerm.toLowerCase)
      val matchingTerms: Map[String, Int] = findAndScoreNgramMatches(trigrams)
      matchingTerms.toSeq.sortBy(_._2).reverse.take(maxCandidatesCount)
    }
  }

}
