/*                                                       *\
**  scala-parser-combinators-completion                  **
**  Copyright (c) by Nexthink S.A.                       **
**  Lausanne, Switzerland (http://www.nexthink.com)      **
**  Author: jonas.chapuis@nexthink.com                   **
\*                                                       */

package com.nexthink.util.parsing.combinator.fuzzy

import com.nexthink.util.collections.PrefixMap
import com.nexthink.util.parsing.combinator.completion.RegexCompletionSupport

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Reader

trait FuzzyParsingHelpers { this: Parsers with RegexCompletionSupport =>

  def remainder[T](reader: Reader[T]): String =
    reader.source.subSequence(reader.pos.column - 1, reader.source.length).toString
  def subSequence[T](reader: Reader[T], end: Int): String = reader.source.subSequence(reader.offset, end).toString

  private def charAtPosition[T](reader: Reader[T], pos: Int): Char = reader.source.charAt(pos).toLower
  private def subSequence[T](reader: Reader[T], start: Int, end: Int): String =
    reader.source.subSequence(start, end).toString
  private def lastPosition[T](reader: Reader[T]): Int = reader.source.length

  protected def findAllMatchingTerms(in: Input, pos: Int, map: PrefixMap[String]): (Seq[String], Int) = {
    def findAllMatchingTermsIter(in: Input, pos: Int, map: PrefixMap[String], prevMatches: Seq[String]): (Seq[String], Int) = {
      lazy val nextSuffixChar = charAtPosition(in, pos)
      if (handleWhiteSpace(in.source, pos) < lastPosition(in) && map.hasSuffix(nextSuffixChar)) {
        findAllMatchingTermsIter(in, pos + 1, map.withPrefix(nextSuffixChar), prevMatches ++ map.value)
      } else {
        (prevMatches ++ map.value, pos)
      }
    }
    findAllMatchingTermsIter(in, pos, map, Seq())
  }
}
