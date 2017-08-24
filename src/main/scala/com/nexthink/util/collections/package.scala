/*                                                       *\
**  scala-parser-combinators-completion                  **
**  Copyright (c) by Nexthink S.A.                       **
**  Lausanne, Switzerland (http://www.nexthink.com)      **
**  Author: jonas.chapuis@nexthink.com                   **
\*                                                       */

import scala.collection.immutable.Set

package object collections {

  implicit class RichIterable[T](iterable: Iterable[T]) {
    def tupled2(): Iterator[(T, T)]    = iterable.grouped(2).collect { case a :: b :: Nil      => (a, b) }
    def tupled3(): Iterator[(T, T, T)] = iterable.grouped(3).collect { case a :: b :: c :: Nil => (a, b, c) }
  }

  object SingleSetElement {
    def unapply[T](s: Set[T]): Option[T] = if (s.size == 1) s.headOption else None
  }
}
