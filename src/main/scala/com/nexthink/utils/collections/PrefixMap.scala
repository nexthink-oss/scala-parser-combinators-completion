package com.nexthink.utils.collections

/**
  * Taken from http://www.scala-lang.org/docu/files/com.nexthink.utils.collections-api/com.nexthink.utils.collections-impl_6.html
  * This is a trie implemented with immutable maps instead of arrays (immutable maps are compact with a small number of elements).
  * If size becomes a problem, we can consider using radix-trees for more compactness,
  * but having char-only nodes intuitively provides the most efficient read performance here since we are either interested
  * in processing the input char-by-char while parsing or looking up n-grams while collecting completions.
  * Subnodes are indeed indexed by char so exploration char by char are very fast, whereas in
  * a radix tree it requires scanning the strings.
  * Another option to look into is a double-array trie, which can be very compact and fast for retrieval https://github.com/digitalstain/DoubleArrayTrie
  */
import scala.collection.immutable
import scala.collection.mutable
import scala.collection.generic.CanBuildFrom

// this is copied from scala-lang, so we don't care about style
// scalastyle:off

class PrefixMap[T] extends mutable.Map[String, T] with mutable.MapLike[String, T, PrefixMap[T]] {

  var suffixes: immutable.Map[Char, PrefixMap[T]] = Map.empty
  var value: Option[T]                            = None

  def get(s: String): Option[T] =
    if (s.isEmpty) value
    else suffixes get s(0) flatMap (_.get(s substring 1))

  def hasSuffix(c: Char): Boolean = suffixes.contains(c)

  def withPrefix(c: Char): PrefixMap[T] =
    suffixes get c match {
      case None => {
        suffixes += (c -> empty)
        suffixes(c)
      }
      case Some(subMap) => subMap
    }

  def withPrefix(s: String): PrefixMap[T] =
    if (s.isEmpty) this
    else {
      val leading = s(0)
      val suffix = suffixes get leading match {
        case None => {
          suffixes += (leading -> empty)
          suffixes(leading)
        }
        case Some(subMap) => subMap
      }
      suffix withPrefix (s substring 1)
    }

  override def update(s: String, elem: T): Unit =
    withPrefix(s).value = Some(elem)

  override def remove(s: String): Option[T] =
    if (s.isEmpty) { val prev = value; value = None; prev } else
      suffixes get s(0) flatMap (_.remove(s substring 1))

  def iterator: Iterator[(String, T)] =
    (for (v          <- value.iterator) yield ("", v)) ++
      (for ((chr, m) <- suffixes.iterator;
            (s, v)   <- m.iterator) yield (chr +: s, v))

  def +=(kv: (String, T)): this.type = { update(kv._1, kv._2); this }

  def -=(s: String): this.type = { remove(s); this }

  override def empty = new PrefixMap[T]
}

object PrefixMap extends {
  def empty[T] = new PrefixMap[T]

  def apply[T](kvs: (String, T)*): PrefixMap[T] = {
    val m: PrefixMap[T] = empty
    for (kv <- kvs) m += kv
    m
  }

  def newBuilder[T]: mutable.Builder[(String, T), PrefixMap[T]] =
    new mutable.MapBuilder[String, T, PrefixMap[T]](empty)

  implicit def canBuildFrom[T]: CanBuildFrom[PrefixMap[_], (String, T), PrefixMap[T]] =
    new CanBuildFrom[PrefixMap[_], (String, T), PrefixMap[T]] {
      def apply(from: PrefixMap[_]) = newBuilder[T]
      def apply()                   = newBuilder[T]
    }
}
