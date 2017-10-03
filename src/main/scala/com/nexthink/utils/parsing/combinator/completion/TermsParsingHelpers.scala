/*                                                       *\
**  scala-parser-combinators-completion                  **
**  Copyright (c) by Nexthink S.A.                       **
**  Lausanne, Switzerland (http://www.nexthink.com)      **
**  Author: jonas.chapuis@nexthink.com                   **
\*                                                       */

package com.nexthink.utils.parsing.combinator.completion

import com.nexthink.utils.collections.PrefixMap

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Reader

trait TermsParsingHelpers { this: RegexParsers =>

  def remainder[T](reader: Reader[T]): String =
    reader.source.subSequence(reader.pos.column - 1, reader.source.length).toString
  def subSequence[T](reader: Reader[T], end: Int): String = reader.source.subSequence(reader.offset, end).toString

  private def charAtPosition[T](reader: Reader[T], pos: Int): Char = reader.source.charAt(pos).toLower
  private def subSequence[T](reader: Reader[T], start: Int, end: Int): String =
    reader.source.subSequence(start, end).toString
  private def lastPosition[T](reader: Reader[T]): Int = reader.source.length

  case class MatchingTerm(term: String, position: Int)

  protected def findAllMatchingTerms(in: Input, pos: Int, map: PrefixMap[String]): (Stream[MatchingTerm], Int) = {
    def findAllMatchingTermsIter(in: Input, pos: Int, map: PrefixMap[String], prevMatches: Stream[MatchingTerm]): (Stream[MatchingTerm], Int) = {
      lazy val nextSuffixChar = charAtPosition(in, pos)
      if (handleWhiteSpace(in.source, pos) < lastPosition(in) && map.hasSuffix(nextSuffixChar)) {
        findAllMatchingTermsIter(in, pos + 1, map.withPrefix(nextSuffixChar), prevMatches ++ map.value.map(MatchingTerm(_, pos)))
      } else {
        (prevMatches ++ map.value.map(MatchingTerm(_, pos)), pos)
      }
    }
    findAllMatchingTermsIter(in, pos, map, Stream())
  }

  protected def findAllTermsWithPrefix(in: Input, pos: Int, map: PrefixMap[String]): Stream[String] = {
    def findAllTermsWithPrefixIter(in: Input, pos: Int, map: PrefixMap[String]): Stream[String] = {
      lazy val nextSuffixChar = charAtPosition(in, pos)
      if (handleWhiteSpace(in.source, pos) < lastPosition(in) && map.hasSuffix(nextSuffixChar)) {
        findAllTermsWithPrefixIter(in, pos + 1, map.withPrefix(nextSuffixChar))
      } else {
        map.toStream.map { case (_, term) => term }
      }
    }
    findAllTermsWithPrefixIter(in, pos, map)
  }
}
