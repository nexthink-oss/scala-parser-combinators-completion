package com.nexthink.utils.parsing.combinator.completion

import io.circe.Encoder

trait RegexCompletionEncoders[M] extends CompletionEncoders[M] with RegexCompletionSupport {
  implicit val elemsEncoder: Encoder[Elems] = Encoder.encodeSeq(Encoder.encodeChar)
}
