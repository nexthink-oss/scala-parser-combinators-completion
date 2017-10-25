package com.nexthink.utils.parsing.combinator.completion

import com.nexthink.utils.collections.{PrefixMap, Trie}

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Reader

trait TermsParsingHelpers { this: RegexParsers =>

  def remainder[T](reader: Reader[T]): String =
    reader.source.subSequence(reader.pos.column - 1, reader.source.length).toString
  def subSequence[T](reader: Reader[T], end: Int): String = reader.source.subSequence(reader.offset, end).toString

  private def charAtPosition[T](reader: Reader[T], pos: Int): Char            = reader.source.charAt(pos).toLower
  private def subSequence[T](reader: Reader[T], start: Int, end: Int): String = reader.source.subSequence(start, end).toString

  case class MatchingTerm(term: String, column: Int)
  case class MatchingTerms(terms: Seq[MatchingTerm], finalColumn: Int)

  protected def findAllMatchingTerms(in: Input, offset: Int, map: Trie): MatchingTerms = {
    def addToSeq(ss: Seq[MatchingTerm], value: String, offset: Int) =
      if (value != null) { // scalastyle:ignore null
        ss :+ MatchingTerm(value, offset+1)
      } else {
        ss
      }
    def findAllMatchingTermsIter(in: Input, offset: Int, map: Trie, prevMatches: Seq[MatchingTerm]): MatchingTerms = {
      if (offset >= in.source.length) {
        MatchingTerms(addToSeq(prevMatches, map.value, offset), offset)
      } else {
        map.getMapForSuffix(charAtPosition(in, offset)) match {
          case Some(subMap) =>
            findAllMatchingTermsIter(in, offset + 1, subMap, addToSeq(prevMatches, map.value, offset))
          case None => MatchingTerms(addToSeq(prevMatches, map.value, offset), offset+1)
        }
      }
    }
    findAllMatchingTermsIter(in, offset, map, Seq())
  }

  protected def findAllTermsWithPrefix(in: Input, offset: Int, map: Trie): Stream[String] = {
    def findAllTermsWithPrefixIter(in: Input, offset: Int, map: Trie): Stream[String] = {
      if (offset >= in.source.length) {
        map.allValues
      } else {
        val currentChar = charAtPosition(in, offset)
        map
          .getMapForSuffix(currentChar)
          .map(
            findAllTermsWithPrefixIter(in, offset + 1, _)
          )
          .getOrElse(Stream())
      }
    }
    findAllTermsWithPrefixIter(in, offset, map)
  }
}
