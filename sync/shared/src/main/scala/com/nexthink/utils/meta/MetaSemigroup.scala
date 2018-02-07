package com.nexthink.utils.meta

trait MetaSemigroup[T] {
  def combine(x: T, y: T): T
}
