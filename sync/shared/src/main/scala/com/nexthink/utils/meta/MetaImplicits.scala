package com.nexthink.utils.meta

trait MetaImplicits {
  implicit val unitSemigroup: MetaSemigroup[Unit] {
    def combine(x: Unit, y: Unit): Unit
  } = new MetaSemigroup[Unit] {
    override def combine(x: Unit, y: Unit): Unit = ()
  }

  implicit val intSemigroup: MetaSemigroup[Int] {
    def combine(x: Int, y: Int): Int
  } = new MetaSemigroup[Int] {
    override def combine(x: Int, y: Int): Int = x + y
  }

  implicit val doubleSemigroup: MetaSemigroup[Double] {
    def combine(x: Double, y: Double): Double
  } = new MetaSemigroup[Double] {
    override def combine(x: Double, y: Double): Double = x + y
  }

  implicit val stringSemigroup: MetaSemigroup[String] {
    def combine(x: String, y: String): String
  } = new MetaSemigroup[String] {
    override def combine(x: String, y: String): String = x + y
  }

  implicit def seqSemigroup[T]: MetaSemigroup[Seq[T]] = (x: Seq[T], y: Seq[T]) => x ++ y

  implicit def mapSemigroup[K, V](implicit valueSemigroup: MetaSemigroup[V]): MetaSemigroup[Map[K, V]] {
    def combine(x: Map[K, V], y: Map[K, V]): Map[K, V]
  } = new MetaSemigroup[Map[K, V]] {
    override def combine(x: Map[K, V], y: Map[K, V]): Map[K, V] = {
      val commonKeys = x.keySet.intersect(y.keySet)
      ((x.keySet -- commonKeys).map(k => k     -> x(k))
        ++ (y.keySet -- commonKeys).map(k => k -> y(k))
        ++ commonKeys.map(k => k               -> valueSemigroup.combine(x(k), y(k)))).toMap
    }
  }
}
