package com.nexthink.utils.meta

trait NxSemigroup[T] {
  def combine(x: T, y: T): T
}
