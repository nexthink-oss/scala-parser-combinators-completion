/*
 * scala-parser-combinators-completion
 * Copyright (c) by Nexthink S.A.
 * Lausanne, Switzerland (http://www.nexthink.com)
 * Author: jonas.chapuis@nexthink.com
 */

package com.nexthink.utils.parsing.combinator.completion

import scala.util.parsing.input.{CharSequenceReader, Position, Reader}

trait RegexCompletionSupportWithExpansion extends RegexCompletionSupport {

  /**
    * Adapts a parser so that completing it will list all possible expanded completions (which successfully parse)
    * (note that if this is used within the context of a grammar allowing for infinitely growing expressions, this
    * will trigger infinite recursion and will end up in a `StackOverflowException`)
    * @param p the parser
    * @tparam T the parser type
    * @return a parser adapter performing completion expansion
    */
  def expandedCompletions[T](p: Parser[T]): Parser[T] = expandedCompletions(p, p)

  /**
    * Adapts a parser so that completing it will construct the list of all possible alternatives up to the point
    * where the passed `stop` parser successfully parses the expansions.
    * (note that if this is used within the context of a grammar allowing for infinitely growing expressions,
    * selecting the relevant stop parser is critical to avoid infinite recursion)
    * @param p the parser
    * @param stop the parser signalling the end of exploration upon successful parse
    * @tparam T the parser type
    * @return a parser adapter performing completion expansion limited according to `stop` parser
    */
  def expandedCompletions[T](p: Parser[T], stop: Parser[T]): Parser[T] =
    Parser(p, in => {
      exploreCompletions(p, stop, in)
    })

  private def exploreCompletions[T](p: Parser[T], stop: Parser[T], in: Input) = {
    def completeString(s: String, position: Int, c: Completion) = s"${s.substring(0, position - 1)} ${c.value}"
    def exploreCompletionsRec(str: String, completions: Completions): Completions = {
      if (completions.isEmpty) completions
      else
        completions.allSets
          .map(cSet => {
            cSet.completions
              .map(c => {
                val completedInput = completeString(str, completions.position.column, c)
                if (stop(new CharSequenceReader(completedInput)).successful) {
                  Completions(in.pos, CompletionSet(cSet.tag, Set(Completion(completedInput, c.score, c.kind))))
                } else {
                  exploreCompletionsRec(completedInput, p.completions(ExplorerReader(p, completedInput)))
                }
              })
              .reduce((a, b) => a | b)
          })
          .reduce((a, b) => a | b)
    }
    if (in match {
          case ExplorerReader(exploredParser, _, _) if exploredParser == p => true
          case _                                                           => false
        }) {
      // recursive parser => avoid infinite exploration
      p.completions(in)
    } else {
      val inputAtPosition = if (in.atEnd) "" else in.rest.source.subSequence(in.offset, in.source.length()).toString.trim
      exploreCompletionsRec(inputAtPosition, p.completions(ExplorerReader(p, inputAtPosition)))
    }
  }

  private case class ExplorerReader(exploredParser: Parser[_], override val source: java.lang.CharSequence, override val offset: Int) extends Reader[Char] {
    val charReader = new CharSequenceReader(source, offset)

    def first = charReader.first

    def rest: ExplorerReader =
      if (offset < source.length) new ExplorerReader(exploredParser, source, offset + 1)
      else this

    def pos: Position = charReader.pos

    def atEnd = charReader.atEnd

    override def drop(n: Int): ExplorerReader = new ExplorerReader(exploredParser, source, offset + n)
  }

  private case object ExplorerReader {
    def apply(exploredParser: Parser[_], source: java.lang.CharSequence): ExplorerReader = ExplorerReader(exploredParser, source, 0)
  }

}
