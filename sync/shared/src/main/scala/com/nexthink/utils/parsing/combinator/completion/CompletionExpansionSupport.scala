package com.nexthink.utils.parsing.combinator.completion

import com.nexthink.utils.meta.NxSemigroup

import scala.util.parsing.input.{CharSequenceReader, OffsetPosition, Position, Reader}

trait CompletionExpansionSupport extends RegexCompletionSupport {

  /**
    * Adapts a parser so that completing it will list all possible expanded completions (which successfully parse)
    * (note that if this is used within the context of a grammar allowing for infinitely growing expressions, this
    * will trigger infinite recursion and will end up in a `StackOverflowException`)
    * @param p the parser
    * @param onlyAtInputEnd expansion happens only when input is positioned exactly at the end upon completion
    * @tparam T the parser type
    * @return a parser adapter performing completion expansion
    */
  def expandedCompletions[T, M](p: Parser[T, M], onlyAtInputEnd: Boolean)(implicit semigroup: NxSemigroup[M]): Parser[T, M] =
    expandedCompletionsWithLimiter(p, p, onlyAtInputEnd)
  def expandedCompletions[T](p: Parser[T, Nothing], onlyAtInputEnd: Boolean = true): Parser[T, Nothing] =
    expandedCompletions[T, Nothing](p, onlyAtInputEnd)

  /**
    * Adapts a parser so that completing it will construct the list of all possible alternatives up to the point
    * where the passed `limiter` parser successfully parses the expansions.
    * (note that if this is used within the context of a grammar allowing for infinitely growing expressions,
    * selecting the relevant limiter parser is critical to avoid infinite recursion)
    * @param p the parser
    * @param onlyAtInputEnd expansion happens only when input is positioned exactly at the end upon completion
    * @param limiter the parser signalling the end of exploration upon successful parse
    * @tparam T the parser type
    * @return a parser adapter performing completion expansion limited according to `limiter` parser
    */
  def expandedCompletionsWithLimiter[T, M](p: Parser[T, M], limiter: Parser[Any, M], onlyAtInputEnd: Boolean = true)(
      implicit semigroup: NxSemigroup[M]): Parser[T, M] =
    Parser(
      p,
      in => {
        lazy val isAtInputEnd = dropAnyWhiteSpace(in).atEnd
        if (!onlyAtInputEnd || isAtInputEnd) {
          val Completions(_, meta, sets) = exploreCompletions(p, limiter, in)
          Completions(OffsetPosition(in.source, handleWhiteSpace(in)), meta, sets)
        } else
          p.completions(in)
      }
    )

  private def exploreCompletions[T, M](p: Parser[T, M], stop: Parser[Any, M], in: Input)(implicit semigroup: NxSemigroup[M]): Completions[M] = {
    def completeString(s: String, position: Int, c: Completion[M]) = {
      val input = s.substring(0, position - 1)
      if (input.trim.isEmpty) c.value.toString() else s"$input ${c.value}"
    }
    def exploreCompletionsRec(str: String, completions: Completions[M]): Completions[M] = {
      if (completions.isEmpty) completions
      else
        completions.allSets
          .map(cSet => {
            cSet.entries
              .map(c => {
                val completedInput = completeString(str, completions.position.column, c)
                if (stop(new CharSequenceReader(completedInput)).successful) {
                  Completions(in.pos, CompletionSet(cSet.tag, Set(Completion(completedInput, c.score, c.meta))))
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

  private case class ExplorerReader(exploredParser: Parser[_, _], override val source: java.lang.CharSequence, override val offset: Int) extends Reader[Char] {
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
    def apply(exploredParser: Parser[_, _], source: java.lang.CharSequence): ExplorerReader = ExplorerReader(exploredParser, source, 0)
  }

}
