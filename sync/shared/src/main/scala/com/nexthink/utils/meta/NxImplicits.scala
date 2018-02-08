package com.nexthink.utils.meta

trait NxImplicits {
  implicit val nothingSemigroup: NxSemigroup[Nothing] = (x: Nothing, y: Nothing) => throw new Exception("never called")
  implicit val intSemigroup: NxSemigroup[Int]       = (x: Int, y: Int) => x + y
  implicit val doubleSemigroup: NxSemigroup[Double] = (x: Double, y: Double) => x + y
  implicit val stringSemigroup: NxSemigroup[String] = (x: String, y: String) => x + y

  implicit def optionSemigroup[T](implicit valueSemigroup: NxSemigroup[T]): NxSemigroup[Option[T]] =
    (x: Option[T], y: Option[T]) =>
      (x, y) match {
        case (None, None)       => None
        case (None, Some(v))    => Some(v)
        case (Some(v), None)    => Some(v)
        case (Some(a), Some(b)) => Some(valueSemigroup.combine(a, b))
    }

  implicit def seqSemigroup[T]: NxSemigroup[Seq[T]] = (x: Seq[T], y: Seq[T]) => x ++ y

  implicit def mapSemigroup[K, V](implicit valueSemigroup: NxSemigroup[V]): NxSemigroup[Map[K, V]] = (x: Map[K, V], y: Map[K, V]) => {
    val commonKeys = x.keySet.intersect(y.keySet)
    ((x.keySet -- commonKeys).map(k => k     -> x(k))
      ++ (y.keySet -- commonKeys).map(k => k -> y(k))
      ++ commonKeys.map(k => k               -> valueSemigroup.combine(x(k), y(k)))).toMap
  }

}
