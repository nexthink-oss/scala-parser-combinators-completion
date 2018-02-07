package com.nexthink.utils.parsing.combinator.completion

import cats._
import cats.implicits._
import com.nexthink.utils.meta.MetaSemigroup
import monix.eval.Task

import scala.collection.mutable.ListBuffer
import scala.language.higherKinds
import scala.language.implicitConversions
import scala.util.parsing.input.Positional
import scala.util.parsing.input.NoPosition

trait AsyncCompletionSupport extends CompletionSupport {
  implicit def semigroupToMeta[T](semigroup: Semigroup[T]): MetaSemigroup[T] {
    def combine(x: T, y: T): T
  } = new MetaSemigroup[T] {
    override def combine(x: T, y: T) = semigroup.combine(x, y) // bridge with cats semigroups since we depend on it
  }
  implicit def completionsMonoid[M](implicit semigroup: MetaSemigroup[M]): Monoid[Completions[M]] =
    new Monoid[Completions[M]] {
      override def empty: Completions[M]                                         = Completions.empty[M]
      override def combine(x: Completions[M], y: Completions[M]): Completions[M] = x | y
    }
  implicit def taskSemigroup[T](implicit ev: Semigroup[T]) = new Semigroup[Task[T]]() {
    override def combine(x: Task[T], y: Task[T]) = Task.mapBoth(x, y)(ev.combine)
  }

  implicit class ConvertibleParser[T, M](p: => Parser[T, M])(implicit semigroup: MetaSemigroup[M]) {
    def toAsync(): AsyncParser[T, M] = parserToAsync(p)
  }
  implicit def parserToAsync[T, M](p: => Parser[T, M])(implicit semigroup: MetaSemigroup[M]): AsyncParser[T, M] = new AsyncParser[T, M] {
    def apply(in: Input)       = Task.eval(p.apply(in))
    def completions(in: Input) = Task.eval(p.completions(in))
  }

  def AsyncParser[T, M](af: Input => Task[ParseResult[T]], ac: Input => Task[Completions[M]])(implicit semigroup: MetaSemigroup[M]): AsyncParser[T, M] =
    new AsyncParser[T, M] {
      def apply(in: Input): Task[ParseResult[T]]                = af(in)
      override def completions(in: Input): Task[Completions[M]] = ac(in)
    }

  abstract class AsyncParser[+T, M](implicit semigroup: MetaSemigroup[M]) extends (Input => Task[ParseResult[T]]) with CombinableParser[T, AsyncParser, M] {
    private var name: String        = ""
    def named(n: String): this.type = { name = n; this }
    override def toString()         = "AsyncParser (" + name + ")"

    def completions(in: Input): Task[Completions[M]]

    private def append[U >: T](p0: => AsyncParser[U, M]): AsyncParser[U, M] = {
      lazy val p = p0
      AsyncParser(
        in => Task.mapBoth(this(in), p0(in))((x, y) => x append y),
        in => {
          val thisCompletions          = this.completions(in)
          lazy val combinedCompletions = thisCompletions |+| p.completions(in)
          for {
            r <- this(in)
            c <- r match {
              case Success(_, rest) =>
                combinedCompletions.map(
                  cc =>
                    // only return any completions if they start at the last position, otherwise it can behave badly e.g. with fuzzy matching
                    if (cc.position < rest.pos) Completions.empty[M]
                    else cc)
              case Failure(_, _) => combinedCompletions
              case Error(_, _) =>
                thisCompletions // avoids backtracking completions in the case of error, e.g. when using the ~! operator
            }
          } yield c
        }
      )
    }

    private def seqCompletions[U](in: Input, other: => AsyncParser[U, M]): Task[Completions[M]] = {
      lazy val thisCompletions = this.completions(in)
      this(in) flatMap {
        case Success(_, rest) => {
          thisCompletions |+| other.completions(rest)
        }
        case NoSuccess(_, _) =>
          thisCompletions
      }
    }

    /** An operator to specify completions of a parser
      * @param completions possible completions for this parser
      * @return a `Parser` that upon invocation of the `completions` method returns the passed completions
      */
    def %>(completions: Elems*): AsyncParser[T, M] =
      %>(CompletionSet(completions.map(el => Completion[M](el))))

    /** An operator to specify completion of a parser
      * @param completion completion for this parser
      * @return a `Parser` that upon invocation of the `completions` method returns the passed completion
      */
    def %>(completion: Completion[M]): AsyncParser[T, M] = %>(CompletionSet(completion))

    /** An operator to specify completions of a parser
      * @param completions possible completions for this parser
      * @return a `Parser` that upon invocation of the `completions` method returns the passed completions
      */
    def %>(completions: CompletionSet[M]): AsyncParser[T, M] =
      AsyncParser(this,
                  in =>
                    this(in).map {
                      case Failure(_, rest) if rest.atEnd =>
                        Completions(rest.pos, completions)
                      case _ => Completions.empty[M]
                  })

    /** An operator to specify completions of a parser
      * @param completioner function of input to completions
      * @return a `Parser` that upon invocation of the `completions` method will invoke the passed function
      */
    def %>(completioner: Input => Task[Completions[M]]): AsyncParser[T, M] =
      AsyncParser(this, completioner)

    /** Limits completions to the top `n` completions ordered by their score
      * @param n the limit
      * @return wrapper `Parser` instance limiting the number of completions
      */
    def topCompletions(n: Int): AsyncParser[T, M] =
      AsyncParser(
        this,
        in => this.completions(in).map(_.takeTop(n))
      )

    /** An operator to specify the completion tag of a parser (empty tag by default)
      * @param tag the completion tag (to be used e.g. to structure a completion menu)
      * @return wrapper `Parser` instance specifying the completion tag
      */
    def %(tag: String): AsyncParser[T, M] =
      AsyncParser(this, in => this.completions(in).map(updateCompletionsTag(_, Some(tag))))

    /** An operator to specify the completions tag score of a parser (0 by default)
      * @param tagScore the completion tag score (to be used e.g. to order sections in a completion menu)
      * @return wrapper `Parser` instance specifying the completion tag score
      */
    def %(tagScore: Int): AsyncParser[T, M] =
      AsyncParser(this, in => this.completions(in).map(updateCompletionsTag(_, None, Some(tagScore))))

    /** An operator to specify the completion tag and score of a parser
      * @param tag the completion tag
      * @param tagScore the completion tag score
      * @return wrapper `Parser` instance specifying the completion tag
      */
    def %(tag: String, tagScore: Int): AsyncParser[T, M] =
      AsyncParser(this, in => this.completions(in).map(updateCompletionsTag(_, Some(tag), Some(tagScore))))

    /** An operator to specify the completion tag, score and description of a parser
      * @param tag the completion tag
      * @param tagScore the completion tag score
      * @param tagDescription the completion tag description
      * @return wrapper `Parser` instance specifying completion tag
      */
    def %(tag: String, tagScore: Int, tagDescription: String): AsyncParser[T, M] =
      AsyncParser(this, in => this.completions(in).map(updateCompletionsTag(_, Some(tag), Some(tagScore), Some(tagDescription))))

    /** An operator to specify the completion tag, score, description and meta of a parser
      * @param tag the completion tag
      * @param tagScore the completion tag score
      * @param tagDescription the completion tag description
      * @param tagMeta the completion tag meta
      * @return wrapper `Parser` instance specifying completion tag
      */
    def %(tag: String, tagScore: Int, tagDescription: String, tagMeta: M): AsyncParser[T, M] =
      AsyncParser(this, in => this.completions(in).map(updateCompletionsTag(_, Some(tag), Some(tagScore), Some(tagDescription), Some(tagMeta))))

    /** An operator to specify the completion tag
      * @param tag the completion tag
      * @return wrapper `Parser` instance specifying completion tag
      */
    def %(tag: CompletionTag[M]): AsyncParser[T, M] =
      AsyncParser(this, in => this.completions(in).map(updateCompletionsTag(_, Some(tag.label), Some(tag.score), tag.description, tag.meta)))

    /** An operator to specify the completion tag description of a parser (empty by default)
      * @param tagDescription the completion description (to be used e.g. to add information to a completion entry)
      * @return wrapper `Parser` instance specifying the completion description
      */
    def %?(tagDescription: String): AsyncParser[T, M] =
      AsyncParser(this, in => this.completions(in).map(updateCompletionsTag(_, None, None, Some(tagDescription))))

    /** An operator to specify the completion tag meta-data of a parser in JSON format (empty by default).
      * JSON meta-data is automatically merged when combining two equivalent tags (i.e. bearing the same label, but with a different payload).
      * This allows for more flexibility when defining the grammar: various parsers can return the same completion tags with
      * an additive effect on the meta-data (and the entries).
      * @param tagMeta the JValue for completion tag meta-data (to be used e.g. to specify the visual style for a completion tag in the menu)
      * @return wrapper `Parser` instance specifying the completion tag meta-data
      */
    def %%(tagMeta: M): AsyncParser[T, M] =
      AsyncParser(this, in => this.completions(in).map(_.map(set => CompletionSet(set.tag.withMeta(tagMeta), set.completions))))

    /** An operator to specify the meta-data for completions of a parser (empty by default).
      * Note that meta-data is merged with comma separations when combining two equivalent entries.
      * @param meta the completion meta-data (to be used e.g. to specify the visual style for a completion entry in the menu)
      * @return wrapper `Parser` instance specifying the completion meta-data
      */
    def %-%(meta: M): AsyncParser[T, M] =
      AsyncParser(this, in => this.completions(in).map(_.map(set => CompletionSet(set.tag, set.entries.map(e => e.withMeta(meta))))))

    /**
      * An operator to specify the meta-data for the whole set of completions (empty by default)
      * Note that if the meta-data is encoded in JSON, it is automatically merged when combining multiple completion sets.
      * This allows for more flexibility when defining the grammar: various parsers can define the global completion meta-data
      * with an additive effect.
      * @param globalMeta the JValue for completions meta-data (to be used e.g. to specify the visual style for the completion menu)
      * @return wrapper `Parser` instance specifying the completions meta-data
      */
    def %%%(globalMeta: M): AsyncParser[T, M] =
      AsyncParser(this, in => this.completions(in).map(_.withMeta(globalMeta)))

    def flatMap[U](f: T => AsyncParser[U, M]): AsyncParser[U, M] =
      AsyncParser(in =>
                    this(in).flatMap {
                      case Success(result, next) => f(result)(next)
                      case ns: NoSuccess         => Task.eval(ns)
                  },
                  completions)

    def map[U](f: T => U): AsyncParser[U, M] =
      AsyncParser(in => this(in).map(_.map(f)), completions)

    def mapCompletions[N](fc: Completions[M] => Completions[N])(implicit semigroup: MetaSemigroup[N]): AsyncParser[T, N] =
      AsyncParser(this, in => this.completions(in).map(fc))

    def map[U, N](f: T => U, fc: Completions[M] => Completions[N])(implicit semigroup: MetaSemigroup[N]): AsyncParser[U, N] =
      AsyncParser(this.map(f), this.mapCompletions(fc).completions)

    def filter(p: T => Boolean): AsyncParser[T, M] = withFilter(p)

    def withFilter(p: T => Boolean): AsyncParser[T, M] =
      AsyncParser(in => {
        for {
          r <- this(in)
        } yield r.filterWithError(p, "Input doesn't match filter: " + _, in)
      }, completions)

    /** A parser combinator for sequential composition.
      *
      * `p ~ q` succeeds if `p` succeeds and `q` succeeds on the input left over by `p`.
      *
      * @param q a parser that will be executed after `p` (this parser)
      *          succeeds -- evaluated at most once, and only when necessary.
      * @return a `Parser` that -- on success -- returns a `~` (like a `Pair`,
      *         but easier to pattern match on) that contains the result of `p` and
      *         that of `q`. The resulting parser fails if either `p` or `q` fails.
      */
    def ~[U](q: => AsyncParser[U, M]): AsyncParser[~[T, U], M] = {
      lazy val p = q
      AsyncParser(for (a <- this; b <- p) yield new ~(a, b), in => seqCompletions(in, p))
    }.named("~")

    /** A parser combinator for sequential composition which keeps only the right result.
      *
      * `p ~> q` succeeds if `p` succeeds and `q` succeeds on the input left over by `p`.
      *
      * @param q a parser that will be executed after `p` (this parser)
      *        succeeds -- evaluated at most once, and only when necessary.
      * @return a `Parser` that -- on success -- returns the result of `q`.
      */
    def ~>[U](q: => AsyncParser[U, M]): AsyncParser[U, M] = {
      lazy val p = q
      AsyncParser(for (_ <- this; b <- p) yield b, in => seqCompletions(in, p))
    }.named("~>")

    /** A parser combinator for sequential composition which keeps only the left result.
      *
      *  `p <~ q` succeeds if `p` succeeds and `q` succeeds on the input
      *           left over by `p`.
      *
      * @note <~ has lower operator precedence than ~ or ~>.
      *
      * @param q a parser that will be executed after `p` (this parser) succeeds -- evaluated at most once, and only when necessary
      * @return a `Parser` that -- on success -- returns the result of `p`.
      */
    def <~[U](q: => AsyncParser[U, M]): AsyncParser[T, M] = {
      lazy val p = q
      AsyncParser(for (a <- this; _ <- p) yield a, in => seqCompletions(in, p))
    }.named("<~")

    /** A parser combinator for non-back-tracking sequential composition.
      *
      *  `p ~! q` succeeds if `p` succeeds and `q` succeeds on the input left over by `p`.
      *   In case of failure, no back-tracking is performed (in an earlier parser produced by the `|` combinator).
      *
      * @param q a parser that will be executed after `p` (this parser) succeeds
      * @return a `Parser` that -- on success -- returns a `~` (like a Pair, but easier to pattern match on)
      *         that contains the result of `p` and that of `q`.
      *         The resulting parser fails if either `p` or `q` fails, this failure is fatal.
      */
    def ~![U](q: => AsyncParser[U, M]): AsyncParser[~[T, U], M] = {
      lazy val p = q
      OnceAsyncParser(for (a <- this; b <- commit(p)) yield new ~(a, b), in => seqCompletions(in, p)).named("~!")
    }

    /** A parser combinator for non-back-tracking sequential composition which only keeps the right result.
      *
      * `p ~>! q` succeeds if `p` succeds and `q` succeds on the input left over by `p`.
      * In case of failure, no back-tracking is performed (in an earlier parser produced by the `|` combinator).
      *
      * @param q a parser that will be executed after `p` (this parser) succeeds -- evaluated at most once, and only when necessary
      * @return a `Parser` that -- on success -- returns the result of `q`.
      *         The resulting parser fails if either `p` or `q` fails, this failure is fatal.
      */
    def ~>![U](q: => AsyncParser[U, M]): AsyncParser[U, M] = {
      lazy val p = q
      OnceAsyncParser(for (_ <- this; b <- commit(p)) yield b, in => seqCompletions(in, p))
    }.named("~>!")

    /** A parser combinator for non-back-tracking sequential composition which only keeps the left result.
      *
      * `p <~! q` succeeds if `p` succeds and `q` succeds on the input left over by `p`.
      * In case of failure, no back-tracking is performed (in an earlier parser produced by the `|` combinator).
      *
      * @param q a parser that will be executed after `p` (this parser) succeeds -- evaluated at most once, and only when necessary
      * @return a `Parser` that -- on success -- reutrns the result of `p`.
      *         The resulting parser fails if either `p` or `q` fails, this failure is fatal.
      */
    def <~![U](q: => AsyncParser[U, M]): AsyncParser[T, M] = {
      lazy val p = q
      OnceAsyncParser(for (a <- this; _ <- commit(p)) yield a, in => seqCompletions(in, p))
    }.named("<~!")

    /** A parser combinator for alternative composition.
      *
      *  `p | q` succeeds if `p` succeeds or `q` succeeds.
      *   Note that `q` is only tried if `p`s failure is non-fatal (i.e., back-tracking is allowed).
      *
      * @param q a parser that will be executed if `p` (this parser) fails (and allows back-tracking)
      * @return a `Parser` that returns the result of the first parser to succeed (out of `p` and `q`)
      *         The resulting parser succeeds if (and only if)
      *         - `p` succeeds, ''or''
      *         - if `p` fails allowing back-tracking and `q` succeeds.
      */
    def |[U >: T](q: => AsyncParser[U, M]): AsyncParser[U, M] =
      append(q).named("|")

    /** A parser combinator for alternative with longest match composition.
      *
      *  `p ||| q` succeeds if `p` succeeds or `q` succeeds.
      *  If `p` and `q` both succeed, the parser that consumed the most characters accepts.
      *
      * @param q a parser that accepts if p consumes less characters. -- evaluated at most once, and only when necessary
      * @return a `Parser` that returns the result of the parser consuming the most characters (out of `p` and `q`).
      */
    def |||[U >: T](q: => AsyncParser[U, M]): AsyncParser[U, M] = {
      lazy val p = q
      AsyncParser(
        in =>
          Task.mapBoth(this(in), q(in))((res1, res2) =>
            (res1, res2) match {
              case (s1 @ Success(_, next1), s2 @ Success(_, next2))    => if (next2.pos < next1.pos) s1 else s2
              case (s1 @ Success(_, _), _)                             => s1
              case (_, s2 @ Success(_, _))                             => s2
              case (e1 @ Error(_, _), _)                               => e1
              case (f1 @ Failure(_, next1), ns2 @ NoSuccess(_, next2)) => if (next2.pos < next1.pos) f1 else ns2
          }),
        in => this.completions(in) |+| p.completions(in)
      )
    }.named("|||")

    /** A parser combinator for function application.
      *
      *  `p ^^ f` succeeds if `p` succeeds; it returns `f` applied to the result of `p`.
      *
      * @param f a function that will be applied to this parser's result (see `map` in `ParseResult`).
      * @return a parser that has the same behaviour as the current parser, but whose result is
      *         transformed by `f`.
      */
    def ^^[U](f: T => U): AsyncParser[U, M] = map(f).named(toString + "^^")

    /** A parser combinator that changes a successful result into the specified value.
      *
      *  `p ^^^ v` succeeds if `p` succeeds; discards its result, and returns `v` instead.
      *
      * @param v The new result for the parser, evaluated at most once (if `p` succeeds), not evaluated at all if `p` fails.
      * @return a parser that has the same behaviour as the current parser, but whose successful result is `v`
      */
    def ^^^[U](v: => U): AsyncParser[U, M] =
      AsyncParser(in => {
        lazy val v0 = v // lazy argument
        this(in).map(_.map(_ => v0))
      }, completions).named(toString + "^^^")

    /** A parser combinator for partial function application.
      *
      *  `p ^? (f, error)` succeeds if `p` succeeds AND `f` is defined at the result of `p`;
      *  in that case, it returns `f` applied to the result of `p`. If `f` is not applicable,
      *  error(the result of `p`) should explain why.
      *
      * @param f a partial function that will be applied to this parser's result
      *          (see `mapPartial` in `ParseResult`).
      * @param error a function that takes the same argument as `f` and produces an error message
      *        to explain why `f` wasn't applicable
      * @return a parser that succeeds if the current parser succeeds <i>and</i> `f` is applicable
      *         to the result. If so, the result will be transformed by `f`.
      */
    def ^?[U](f: PartialFunction[T, U], error: T => String): AsyncParser[U, M] =
      AsyncParser(in => this(in).map(_.mapPartial(f, error)), completions).named(toString + "^?")

    /** A parser combinator for partial function application.
      *
      *  `p ^? f` succeeds if `p` succeeds AND `f` is defined at the result of `p`;
      *  in that case, it returns `f` applied to the result of `p`.
      *
      * @param f a partial function that will be applied to this parser's result
      *          (see `mapPartial` in `ParseResult`).
      * @return a parser that succeeds if the current parser succeeds <i>and</i> `f` is applicable
      *         to the result. If so, the result will be transformed by `f`.
      */
    def ^?[U](f: PartialFunction[T, U]): AsyncParser[U, M] =
      ^?(f, r => "Constructor function not defined at " + r)

    /** A parser combinator that parameterizes a subsequent parser with the
      *  result of this one.
      *
      *  Use this combinator when a parser depends on the result of a previous
      *  parser. `p` should be a function that takes the result from the first
      *  parser and returns the second parser.
      *
      *  `p into fq` (with `fq` typically `{x => q}`) first applies `p`, and
      *  then, if `p` successfully returned result `r`, applies `fq(r)` to the
      *  rest of the input.
      *
      *  ''From: G. Hutton. Higher-order functions for parsing. J. Funct. Program., 2(3):323--343, 1992.''
      *
      *  @example {{{
      *  def perlRE = "m" ~> (".".r into (separator => """[^%s]*""".format(separator).r <~ separator))
      *  }}}
      *
      *  @param fq a function that, given the result from this parser, returns
      *         the second parser to be applied
      *  @return a parser that succeeds if this parser succeeds (with result `x`)
      *          and if then `fq(x)` succeeds
      */
    def into[U](fq: T => AsyncParser[U, M]): AsyncParser[U, M] =
      AsyncParser(flatMap(fq),
                  in =>
                    this(in).flatMap {
                      case Success(result, next) => fq(result).completions(next)
                      case _: NoSuccess          => this.completions(in)
                  })

    /** Returns `into(fq)`. */
    def >>[U](fq: T => AsyncParser[U, M]) = into(fq)

    /** Returns a parser that repeatedly parses what this parser parses.
      *
      *  @return rep(this)
      */
    def * = rep(this)

    /** Returns a parser that repeatedly parses what this parser parses,
      * interleaved with the `sep` parser. The `sep` parser specifies how
      * the results parsed by this parser should be combined.
      *
      * @return chainl1(this, sep)
      */
    def *[U >: T](sep: => AsyncParser[(U, U) => U, M]) = chainl1(this, sep)

    /** Returns a parser that repeatedly (at least once) parses what this parser parses.
      *
      *  @return rep1(this)
      */
    def +() = rep1(this)

    /** Returns a parser that optionally parses what this parser parses.
      *
      *  @return opt(this)
      */
    def ?() = opt(this)

    /** Changes the failure message produced by a parser.
      *
      *  This doesn't change the behavior of a parser on neither
      *  success nor error, just on failure. The semantics are
      *  slightly different than those obtained by doing `| failure(msg)`,
      *  in that the message produced by this method will always
      *  replace the message produced, which is not guaranteed
      *  by that idiom.
      *
      *  For example, parser `p` below will always produce the
      *  designated failure message, while `q` will not produce
      *  it if `sign` is parsed but `number` is not.
      *
      *  {{{
      *  def p = sign.? ~ number withFailureMessage  "Number expected!"
      *  def q = sign.? ~ number | failure("Number expected!")
      *  }}}
      *
      *  @param msg The message that will replace the default failure message.
      *  @return    A parser with the same properties and different failure message.
      */
    def withFailureMessage(msg: String): AsyncParser[T, M] =
      AsyncParser(in =>
                    this(in).map {
                      case Failure(_, next) => Failure(msg, next)
                      case other            => other
                  },
                  completions)

    /** Changes the failure message produced by a parser.
      *
      *  This doesn't change the behavior of a parser on neither
      *  success nor error, just on failure. The semantics are
      *  slightly different than those obtained by doing `| failure(msg)`,
      *  in that the message produced by this method will always
      *  replace the message produced, which is not guaranteed
      *  by that idiom.
      *
      *  For example, parser `p` below will always produce the
      *  designated failure message, while `q` will not produce
      *  it if `sign` is parsed but `number` is not.
      *
      *  {{{
      *  def p = sign.? ~ number withFailureMessage  "Number expected!"
      *  def q = sign.? ~ number | failure("Number expected!")
      *  }}}
      *
      *  @param msg The message that will replace the default failure message.
      *  @return    A parser with the same properties and different failure message.
      */
    def withErrorMessage(msg: String): AsyncParser[T, M] =
      AsyncParser(in =>
                    this(in).map {
                      case Error(_, next) => Error(msg, next)
                      case other          => other
                  },
                  completions)
  }

  /** Wrap a parser so that its failures become errors (the `|` combinator
    *  will give up as soon as it encounters an error, on failure it simply
    *  tries the next alternative).
    */
  def commit[T, M](p: => AsyncParser[T, M])(implicit semigroup: MetaSemigroup[M]) =
    AsyncParser(in =>
                  p(in).map {
                    case s @ Success(_, _)  => s
                    case e @ Error(_, _)    => e
                    case Failure(msg, next) => Error(msg, next)
                },
                p.completions)

  /** A helper method that turns a `Parser` into one that will
    * print debugging information to stdout before and after
    * being applied.
    */
  def log[T, M](p: => AsyncParser[T, M])(name: String)(implicit semigroup: MetaSemigroup[M]): AsyncParser[T, M] =
    AsyncParser(
      in => {
        println(s"trying $name at \n${inputPositionDebugString(in)}")
        p(in).map(r => {
          println(s"$name --> $r")
          r
        })
      },
      in => {
        println(s"completing $name at \n${inputPositionDebugString(in)}")
        p.completions(in)
          .map(r => {
            println(s"$name --> $r")
            r
          })
      }
    )

  /** A parser generator for repetitions.
    *
    * `rep(p)` repeatedly uses `p` to parse the input until `p` fails
    * (the result is a List of the consecutive results of `p`).
    *
    * @param p a `Parser` that is to be applied successively to the input
    * @return A parser that returns a list of results produced by repeatedly applying `p` to the input.
    */
  def rep[T, M](p: => AsyncParser[T, M])(implicit semigroup: MetaSemigroup[M]): AsyncParser[List[T], M] = {
    rep1(p) | success[List[T], M](List[T]())
  }

  /** A parser generator for interleaved repetitions.
    *
    *  `repsep(p, q)` repeatedly uses `p` interleaved with `q` to parse the input, until `p` fails.
    *  (The result is a `List` of the results of `p`.)
    *
    *  Example: `repsep(term, ",")` parses a comma-separated list of term's, yielding a list of these terms.
    *
    * @param p a `Parser` that is to be applied successively to the input
    * @param q a `Parser` that parses the elements that separate the elements parsed by `p`
    * @return A parser that returns a list of results produced by repeatedly applying `p` (interleaved with `q`) to the input.
    *         The results of `p` are collected in a list. The results of `q` are discarded.
    */
  def repsep[T, M](p: => AsyncParser[T, M], q: => AsyncParser[T, M])(implicit semigroup: MetaSemigroup[M]): AsyncParser[List[T], M] = {
    rep1sep(p, q) | success[List[T], M](List[T]())
  }

  /** A parser generator for non-empty repetitions.
    *
    *  `rep1(p)` repeatedly uses `p` to parse the input until `p` fails -- `p` must succeed at least
    *             once (the result is a `List` of the consecutive results of `p`)
    *
    * @param p a `Parser` that is to be applied successively to the input
    * @return A parser that returns a list of results produced by repeatedly applying `p` to the input
    *        (and that only succeeds if `p` matches at least once).
    */
  def rep1[T, M](p: => AsyncParser[T, M])(implicit semigroup: MetaSemigroup[M]): AsyncParser[List[T], M] =
    rep1(p, p)

  /** A parser generator for non-empty repetitions.
    *
    *  `rep1(f, p)` first uses `f` (which must succeed) and then repeatedly
    *     uses `p` to parse the input until `p` fails
    *     (the result is a `List` of the consecutive results of `f` and `p`)
    *
    * @param first a `Parser` that parses the first piece of input
    * @param q a `Parser` that is to be applied successively to the rest of the input (if any) -- evaluated at most once, and only when necessary
    * @return A parser that returns a list of results produced by first applying `f` and then
    *         repeatedly `p` to the input (it only succeeds if `f` matches).
    */
  def rep1[T, M](first: => AsyncParser[T, M], q: => AsyncParser[T, M])(implicit semigroup: MetaSemigroup[M]): AsyncParser[List[T], M] = {
    lazy val p = q // lazy argument
    AsyncParser(
      in => {
        val elems = new ListBuffer[T]
        def continue(in: Input): Task[ParseResult[List[T]]] = {
          p(in) flatMap {
            case Success(x, rest) => elems += x; continue(rest)
            case e @ Error(_, _)  => Task.eval(e) // still have to propagate error
            case _                => Task.eval(Success(elems.toList, in))
          }
        }
        first(in) flatMap {
          case Success(x, rest) => elems += x; continue(rest)
          case ns: NoSuccess    => Task.eval(ns)
        }
      },
      in => {
        def continue(in: Input): Task[Completions[M]] = {
          val currentCompletions = p.completions(in)
          p(in) flatMap {
            case Success(_, rest) => currentCompletions |+| continue(rest)
            case NoSuccess(_, _)  => currentCompletions
          }
        }
        val firstCompletions = first.completions(in)
        first(in) flatMap {
          case Success(_, rest) => firstCompletions |+| continue(rest)
          case NoSuccess(_, _)  => firstCompletions
        }
      }
    )
  }

  /** A parser generator for a specified number of repetitions.
    *
    *  `repN(n, p)` uses `p` exactly `n` time to parse the input
    *  (the result is a `List` of the `n` consecutive results of `p`).
    *
    * @param q   a `Parser` that is to be applied successively to the input
    * @param num the exact number of times `p` must succeed
    * @return    A parser that returns a list of results produced by repeatedly applying `p` to the input
    *        (and that only succeeds if `p` matches exactly `n` times).
    */
  def repN[T, M](num: Int, q: => AsyncParser[T, M])(implicit semigroup: MetaSemigroup[M]): AsyncParser[List[T], M] = {
    val p = q // avoid repeatedly re-evaluating by-name parser
    if (num == 0) success[List[T], M](Nil)
    else
      AsyncParser(
        in => {
          val elems = new ListBuffer[T]
          def applyp(in0: Input): Task[ParseResult[List[T]]] =
            if (elems.length == num) Task.eval(Success(elems.toList, in0))
            else
              p(in0) flatMap {
                case Success(x, rest) => elems += x; applyp(rest)
                case ns: NoSuccess    => Task.eval(ns)
              }

          applyp(in)
        },
        in => {
          var parsedCount = 0
          def completions(in0: Input): Task[Completions[M]] =
            if (parsedCount == num) {
              Task.eval(Completions.empty[M])
            } else {
              val currentCompletions = p.completions(in0)
              p(in0) flatMap {
                case Success(_, rest) => parsedCount += 1; currentCompletions |+| completions(rest)
                case _: NoSuccess     => currentCompletions
              }
            }
          completions(in).map(r => if (parsedCount < num) r else Completions.empty[M])
        }
      )
  }

  /** A parser generator for non-empty repetitions.
    *
    *  `rep1sep(p, q)` repeatedly applies `p` interleaved with `q` to parse the
    *  input, until `p` fails. The parser `p` must succeed at least once.
    *
    * @param p a `Parser` that is to be applied successively to the input
    * @param q a `Parser` that parses the elements that separate the elements parsed by `p`
    *          (interleaved with `q`)
    * @return A parser that returns a list of results produced by repeatedly applying `p` to the input
    *         (and that only succeeds if `p` matches at least once).
    *         The results of `p` are collected in a list. The results of `q` are discarded.
    */
  def rep1sep[T, M](p: => AsyncParser[T, M], q: => AsyncParser[T, M])(implicit semigroup: MetaSemigroup[M]): AsyncParser[List[T], M] =
    p ~ rep(q ~> p) ^^ { case x ~ y => x :: y }

  /** A parser generator that, roughly, generalises the rep1sep generator so
    *  that `q`, which parses the separator, produces a left-associative
    *  function that combines the elements it separates.
    *
    *  ''From: J. Fokker. Functional parsers. In J. Jeuring and E. Meijer, editors, Advanced Functional Programming,
    *  volume 925 of Lecture Notes in Computer Science, pages 1--23. Springer, 1995.''
    *
    * @param p a parser that parses the elements
    * @param q a parser that parses the token(s) separating the elements, yielding a left-associative function that
    *          combines two elements into one
    */
  def chainl1[T, M](p: => AsyncParser[T, M], q: => AsyncParser[(T, T) => T, M])(implicit semigroup: MetaSemigroup[M]): AsyncParser[T, M] =
    chainl1(p, p, q)

  /** A parser generator that, roughly, generalises the `rep1sep` generator
    *  so that `q`, which parses the separator, produces a left-associative
    *  function that combines the elements it separates.
    *
    * @param first a parser that parses the first element
    * @param p a parser that parses the subsequent elements
    * @param q a parser that parses the token(s) separating the elements,
    *          yielding a left-associative function that combines two elements
    *          into one
    */
  def chainl1[T, U, M](first: => AsyncParser[T, M], p: => AsyncParser[U, M], q: => AsyncParser[(T, U) => T, M])(
      implicit semigroup: MetaSemigroup[M]): AsyncParser[T, M] =
    first ~ rep(q ~ p) ^^ {
      case x ~ xs =>
        xs.foldLeft(x: T) { case (a, f ~ b) => f(a, b) } // x's type annotation is needed to deal with changed type inference due to SI-5189
    }

  /** A parser generator that generalises the `rep1sep` generator so that `q`,
    *  which parses the separator, produces a right-associative function that
    *  combines the elements it separates. Additionally, the right-most (last)
    *  element and the left-most combining function have to be supplied.
    *
    * rep1sep(p: Parser[T], q) corresponds to chainr1(p, q ^^ cons, cons, Nil) (where val cons = (x: T, y: List[T]) => x :: y)
    *
    * @param p a parser that parses the elements
    * @param q a parser that parses the token(s) separating the elements, yielding a right-associative function that
    *          combines two elements into one
    * @param combine the "last" (left-most) combination function to be applied
    * @param first   the "first" (right-most) element to be combined
    */
  def chainr1[T, U, M](p: => AsyncParser[T, M], q: => AsyncParser[(T, U) => U, M], combine: (T, U) => U, first: U)(
      implicit semigroup: MetaSemigroup[M]): AsyncParser[U, M] =
    p ~ rep(q ~ p) ^^ {
      case x ~ xs =>
        (new ~(combine, x) :: xs).foldRight(first) { case (f ~ a, b) => f(a, b) }
    }

  /** A parser generator for optional sub-phrases.
    *
    *  `opt(p)` is a parser that returns `Some(x)` if `p` returns `x` and `None` if `p` fails.
    *
    * @param p A `Parser` that is tried on the input
    * @return a `Parser` that always succeeds: either with the result provided by `p` or
    *         with the empty result
    */
  def opt[T, M](p: => AsyncParser[T, M])(implicit semigroup: MetaSemigroup[M]): AsyncParser[Option[T], M] = {
    val empty: Parser[Option[T], M] = success(Option.empty[T])
    p ^^ (x => Some(x)) | empty
  }

  /** Wrap a parser so that its failures and errors become success and
    *  vice versa -- it never consumes any input.
    */
  def not[T, M](p: => AsyncParser[T, M])(implicit semigroup: MetaSemigroup[M]): AsyncParser[Unit, M] =
    AsyncParser(in =>
                  p(in) map {
                    case Success(_, rest) => Failure("Expected failure", rest)
                    case _                => Success((), in)
                },
                _ => Task.eval(Completions.empty[M]))

  /** A parser generator for guard expressions. The resulting parser will
    *  fail or succeed just like the one given as parameter but it will not
    *  consume any input.
    *
    * @param p a `Parser` that is to be applied to the input
    * @return A parser that returns success if and only if `p` succeeds but
    *         never consumes any input
    */
  def guard[T, M](p: => AsyncParser[T, M])(implicit semigroup: MetaSemigroup[M]): AsyncParser[T, M] =
    AsyncParser(in =>
                  p(in) map {
                    case Success(r, _) => Success(r, in)
                    case e             => e
                },
                p.completions)

  /** `positioned` decorates a parser's result with the start position of the
    *  input it consumed.
    *
    * @param p a `Parser` whose result conforms to `Positional`.
    * @return A parser that has the same behaviour as `p`, but which marks its
    *         result with the start position of the input it consumed,
    *         if it didn't already have a position.
    */
  def positioned[T <: Positional, M](p: => AsyncParser[T, M])(implicit semigroup: MetaSemigroup[M]): AsyncParser[T, M] =
    AsyncParser(in =>
                  p(in) map {
                    case Success(t, in1) => Success(if (t.pos == NoPosition) t setPos in.pos else t, in1)
                    case ns: NoSuccess   => ns
                },
                p.completions)

  def OnceAsyncParser[T, M](af: Input => Task[ParseResult[T]], ac: Input => Task[Completions[M]])(implicit semigroup: MetaSemigroup[M]): OnceAsyncParser[T, M] =
    new OnceAsyncParser[T, M] {
      override def completions(in: Input) = ac(in)
      override def apply(in: Input)       = af(in)
    }

  /** A parser whose `~` combinator disallows back-tracking.
    */
  abstract class OnceAsyncParser[+T, M](implicit semigroup: MetaSemigroup[M]) extends AsyncParser[T, M] {
    override def ~[U](p: => AsyncParser[U, M]): AsyncParser[~[T, U], M] =
      AsyncParser(for (a <- this; b <- commit(p)) yield new ~(a, b), super.~(p).completions).named("~")
  }

}
