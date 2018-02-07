package com.nexthink.utils.parsing.combinator.completion

import io.circe.Encoder
import io.circe.ObjectEncoder

import scala.util.parsing.input.Position

trait CompletionEncoders[M] extends CompletionTypes {
  implicit val elemsEncoder: Encoder[Elems]
  implicit val metaEncoder: Encoder[M]

  implicit def encodeTag: ObjectEncoder[CompletionTag[M]] =
    Encoder.forProduct4("label", "score", "description", "meta")(t => (t.label, t.score, t.description, t.meta))

  implicit def setEncoder: ObjectEncoder[CompletionSet[M]] =
    Encoder.forProduct2("tag", "completions")(s => (s.tag, s.completions.toSeq))

  implicit def completionEncoder: ObjectEncoder[Completion[M]] =
    Encoder.forProduct3("value", "score", "meta")(c => (c.value, c.score, c.meta))

  implicit def completionsEncoder: ObjectEncoder[Completions[M]] =
    Encoder.forProduct3("position", "meta", "sets")(c => (c.position, c.meta, c.allSets))

  implicit val positionEncoder: Encoder[Position] = Encoder.forProduct2("line", "column")(p => (p.line, p.column))
}
