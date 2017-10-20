/*
 * scala-parser-combinators-completion
 * Copyright (c) by Nexthink S.A.
 * Lausanne, Switzerland (http://www.nexthink.com)
 * Author: jonas.chapuis@nexthink.com
 */

package com.nexthink.utils.collections

import java.util

class Trie private {
  private var keys: Array[Char]   = Array()
  private var values: Array[Trie] = Array()
  var value: String               = _

  def getMapForSuffix(c: Char): Option[Trie] = {
    val lowerC = c.toLower
    var i = 0
    while (i < keys.length) {
      if (keys(i) == lowerC) {
        return Some(values(i))
      }
      i = i + 1
    }
    None
  }

  def allValues: Stream[String] = {
    val subValues = values.toStream.flatMap(_.allValues)
    if (value != null) {
      value #:: subValues
    } else {
      subValues
    }
  }

  private def withPrefix(s: String): Trie = {
    if (s.isEmpty) {
      this
    } else {
      val leading = s(0).toLower
      val suffix = getMapForSuffix(leading) match {
        case None => {
          val newKeys   = util.Arrays.copyOf(keys, keys.length + 1)
          val newValues = util.Arrays.copyOf(values, values.length + 1)
          newKeys(newKeys.length - 1) = leading
          val trie = new Trie
          newValues(newValues.length - 1) = trie
          keys = newKeys
          values = newValues
          trie
        }
        case Some(subMap) => subMap
      }
      suffix.withPrefix(s.substring(1))
    }
  }

  private def update(s: String, elem: String): Unit = withPrefix(s).value = elem
}

object Trie {
  def apply(kvs: (String, String)*): Trie = {
    val m: Trie = new Trie
    for (kv <- kvs) {
      m.update(kv._1, kv._2)
    }
    m
  }
}
