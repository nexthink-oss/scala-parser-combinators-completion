package com.nexthink.utils.parsing.combinator.completion

import monix.eval.Task
import monix.execution.Scheduler

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.matching.Regex
import scala.util.parsing.input.{CharSequenceReader, PagedSeq, PagedSeqReader, Reader}

trait AsyncRegexCompletionSupport extends RegexCompletionSupport with AsyncCompletionSupport {

  /** Async explicits that simply wrap the corresponding parsers into direct eval tasks **/
  implicit def asyncLiteral(s: String): AsyncParser[String, Nothing] = parserToAsync(super.literal(s))
  implicit def asyncRegex(r: Regex): AsyncParser[String, Nothing]    = parserToAsync(super.regex(r))

  /** Parse some prefix of reader `in` with parser `p`. */
  def parseAsync[T, M](p: AsyncParser[T, M], in: Reader[Char]): Task[ParseResult[T]] =
    p(in)

  /** Parse some prefix of character sequence `in` with parser `p`. */
  def parseAsync[T, M](p: AsyncParser[T, M], in: java.lang.CharSequence): Task[ParseResult[T]] =
    p(new CharSequenceReader(in))

  /** Parse some prefix of reader `in` with parser `p`. */
  def parseAsync[T, M](p: AsyncParser[T, M], in: java.io.Reader): Task[ParseResult[T]] =
    p(new PagedSeqReader(PagedSeq.fromReader(in)))

  /** Returns completions for read `in` with parser `p`. */
  def completeAsync[T, M](p: AsyncParser[T, M], in: Reader[Char]): Task[Completions[M]] =
    p.completions(in)

  /** Returns completions for character sequence `in` with parser `p`. */
  def completeAsync[T, M](p: AsyncParser[T, M], in: CharSequence): Task[Completions[M]] =
    p.completions(new CharSequenceReader(in))

  /** Returns flattened string completions for character sequence `in` with parser `p`. */
  def completeStringAsync[T, M](p: AsyncParser[T, M], input: String): Task[Seq[String]] =
    completeAsync(p, input).map(_.completionStrings)

  def parse[T, M](p: AsyncParser[T, M], in: Reader[Char])(implicit s: Scheduler): ParseResult[T] = Await.result(parseAsync(p, in).runAsync(s), Duration.Inf)

  def parse[T, M](p: AsyncParser[T, M], in: CharSequence)(implicit s: Scheduler) = Await.result(parseAsync(p, in).runAsync(s), Duration.Inf)

  def parse[T, M](p: AsyncParser[T, M], in: java.io.Reader)(implicit s: Scheduler) = Await.result(parseAsync(p, in).runAsync(s), Duration.Inf)

  /** Returns completions for read `in` with parser `p`. */
  def complete[T, M](p: AsyncParser[T, M], in: Reader[Char])(implicit s: Scheduler) = Await.result(completeAsync(p, in).runAsync(s), Duration.Inf)

  /** Returns completions for character sequence `in` with parser `p`. */
  def complete[T, M](p: AsyncParser[T, M], in: CharSequence)(implicit s: Scheduler) = Await.result(completeAsync(p, in).runAsync(s), Duration.Inf)

  /** Returns flattened string completions for character sequence `in` with parser `p`. */
  def completeString[T, M](p: AsyncParser[T, M], input: String)(implicit s: Scheduler) = Await.result(completeStringAsync(p, input).runAsync(s), Duration.Inf)
}
