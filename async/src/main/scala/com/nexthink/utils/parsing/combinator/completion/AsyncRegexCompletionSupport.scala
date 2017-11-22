package com.nexthink.utils.parsing.combinator.completion

import monix.eval.Task
import monix.execution.Scheduler

import scala.collection.immutable.PagedSeq
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.matching.Regex
import scala.util.parsing.input.Reader
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.input.PagedSeqReader

trait AsyncRegexCompletionSupport extends RegexCompletionSupport with AsyncCompletionSupport {

  /** Async explicits that simply wrap the corresponding parsers into direct eval tasks **/
  implicit def asyncLiteral(s: String): AsyncParser[String] = parserToAsync(super.literal(s))
  implicit def asyncRegex(r: Regex): AsyncParser[String]    = parserToAsync(super.regex(r))

  /** Parse some prefix of reader `in` with parser `p`. */
  def parseAsync[T](p: AsyncParser[T], in: Reader[Char]): Task[ParseResult[T]] =
    p(in)

  /** Parse some prefix of character sequence `in` with parser `p`. */
  def parseAsync[T](p: AsyncParser[T], in: java.lang.CharSequence): Task[ParseResult[T]] =
    p(new CharSequenceReader(in))

  /** Parse some prefix of reader `in` with parser `p`. */
  def parseAsync[T](p: AsyncParser[T], in: java.io.Reader): Task[ParseResult[T]] =
    p(new PagedSeqReader(PagedSeq.fromReader(in)))

  /** Returns completions for read `in` with parser `p`. */
  def completeAsync[T](p: AsyncParser[T], in: Reader[Char]): Task[Completions] =
    p.completions(in)

  /** Returns completions for character sequence `in` with parser `p`. */
  def completeAsync[T](p: AsyncParser[T], in: CharSequence): Task[Completions] =
    p.completions(new CharSequenceReader(in))

  /** Returns flattened string completions for character sequence `in` with parser `p`. */
  def completeStringAsync[T](p: AsyncParser[T], input: String): Task[Seq[String]] =
    completeAsync(p, input).map(_.completionStrings)

  def parse[T](p: AsyncParser[T], in: Reader[Char])(implicit s: Scheduler): ParseResult[T] = Await.result(parseAsync(p, in).runAsync(s), Duration.Inf)

  def parse[T](p: AsyncParser[T], in: CharSequence)(implicit s: Scheduler) = Await.result(parseAsync(p, in).runAsync(s), Duration.Inf)

  def parse[T](p: AsyncParser[T], in: java.io.Reader)(implicit s: Scheduler) = Await.result(parseAsync(p, in).runAsync(s), Duration.Inf)

  /** Returns completions for read `in` with parser `p`. */
  def complete[T](p: AsyncParser[T], in: Reader[Char])(implicit s: Scheduler) = Await.result(completeAsync(p, in).runAsync(s), Duration.Inf)

  /** Returns completions for character sequence `in` with parser `p`. */
  def complete[T](p: AsyncParser[T], in: CharSequence)(implicit s: Scheduler) = Await.result(completeAsync(p, in).runAsync(s), Duration.Inf)

  /** Returns flattened string completions for character sequence `in` with parser `p`. */
  def completeString[T](p: AsyncParser[T], input: String)(implicit s: Scheduler) = Await.result(completeStringAsync(p, input).runAsync(s), Duration.Inf)
}
