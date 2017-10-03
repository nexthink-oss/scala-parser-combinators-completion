/*                                                       *\
**  scala-parser-combinators-completion                  **
**  Copyright (c) by Nexthink S.A.                       **
**  Lausanne, Switzerland (http://www.nexthink.com)      **
**  Author: jonas.chapuis@nexthink.com                   **
\*                                                       */

package com.nexthink.utils.parsing.combinator.completion

import com.nexthink.utils.collections.{PrefixMap, Trie}

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

  protected def findAllMatchingTerms(in: Input, pos: Int, map: Trie): (Seq[String], Int) = {
    def addToSeq(ss: Seq[String], value: String) =
      if (value != null) { // scalastyle:ignore null
        ss :+ value
      } else {
        ss
      }
    def findAllMatchingTermsIter(in: Input, pos: Int, map: Trie, prevMatches: Seq[String]): (Seq[String], Int) = {
      lazy val nextSuffixChar = charAtPosition(in, pos)
      if (handleWhiteSpace(in.source, pos) < lastPosition(in)) {
        map.getMapForSuffix(nextSuffixChar) match {
          case Some(subMap) => findAllMatchingTermsIter(in, pos + 1, subMap, addToSeq(prevMatches, map.value))
          case None         => (addToSeq(prevMatches, map.value), pos)
        }
      } else {
        (addToSeq(prevMatches, map.value), pos)
      }
    }
    findAllMatchingTermsIter(in, pos, map, Seq())
  }

  protected def findAllTermsWithPrefix(in: Input, pos: Int, map: Trie): Stream[String] = {
    def findAllTermsWithPrefixIter(in: Input, pos: Int, map: Trie): Stream[String] = {
      lazy val nextSuffixChar = charAtPosition(in, pos)
      lazy val subMap = map.getMapForSuffix(nextSuffixChar)
      if (handleWhiteSpace(in.source, pos) < lastPosition(in) && subMap.isDefined) {
        findAllTermsWithPrefixIter(in, pos + 1, subMap.get)
      } else {
        map.allValues
      }
    }
    findAllTermsWithPrefixIter(in, pos, map)
  }
}
