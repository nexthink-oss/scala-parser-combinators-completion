package com.nexthink.utils.parsing.combinator.completion
import cats.Eq
import cats.tests.CatsSuite
import cats.kernel.laws.GroupLaws
import org.json4s.JValue
import org.scalacheck.Arbitrary._
import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.ScalacheckShapeless._

import scala.util.parsing.input.OffsetPosition

class CompletionsLawsTest extends CatsSuite with AsyncCompletionSupport {
  type Elem = Char
  val nonEmptyElems                                        = Gen.oneOf("foo", "bar", "asd", "efg").map(_.toSeq)
  implicit def arbElems: Arbitrary[Elems]                  = Arbitrary(nonEmptyElems)
  implicit def eqCompletions[Completions]: Eq[Completions] = Eq.fromUniversalEquals
  implicit def arbCharSequence: Arbitrary[CharSequence] =
    Arbitrary(for {
      seq <- Gen.alphaStr.map(_.toCharArray)
    } yield seq)
  implicit def arbCompletionSet: Arbitrary[CompletionSet] =
    Arbitrary(for {
      tag         <- arbitrary[CompletionTag]
      completions <- arbitrary[List[Completion]]
    } yield CompletionSet(tag, completions))
  implicit def arbCompletions: Arbitrary[Completions] =
    Arbitrary(for {
      position <- arbitrary[OffsetPosition]
      meta     <- arbitrary[Option[JValue]]
      sets     <- arbitrary[List[CompletionSet]]
    } yield Completions(position, meta, sets))

  checkAll("Completions.MonoidLaws", GroupLaws[Completions].monoid)
}
