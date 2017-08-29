/*
 * scala-parser-combinators-completion
 * Copyright (c) by Nexthink S.A.
 * Lausanne, Switzerland (http://www.nexthink.com)
 * Author: jonas.chapuis@nexthink.com
 */

package com.nexthink.utils.parsing.combinator.completion

import scala.util.parsing.input.{CharSequenceReader, Position, Reader}

trait RegexCompletionSupportWithExpansion extends RegexCompletionSupport {

  def expandedCompletions[T](p: Parser[T]): Parser[T]                  = expandedCompletions(p, p)
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
            val completionPos                     = completions.position.column
            val inputCompletedWithFirstSetElement = ExplorerReader(p, completeString(str, completionPos, cSet.completions.head))
            if (stop(inputCompletedWithFirstSetElement).successful) {
              // we consider we have reached stop condition with this completion set
              Completions(in.pos, CompletionSet(cSet.tag, cSet.completions.map(c => Completion(completeString(str, completionPos, c), c.score, c.kind))))
            } else {
              // continue exploring
              cSet.completions
                .map(c => {
                  val completedInput = completeString(str, completionPos, c)
                  println(completedInput)
                  exploreCompletionsRec(completedInput, p.completions(ExplorerReader(p, completedInput)))
                })
                .reduce((a, b) => a | b)
            }
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
