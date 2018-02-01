package com.nexthink.utils.parsing

package object distance {
  def tokenizeWords(s: String): Array[String] = s.split("\\s+")

  def affix(string: String)(arity: Int): String = {
    val affixChar = '_'
    val affix     = List.fill(arity - 1)(affixChar).mkString
    affix + string + affix
  }

  def trigramsWithAffixing(string: String): Seq[String] = ngramsWithAffixing(string)(3)

  def ngramsWithAffixing(string: String)(arity: Int): Seq[String] = {
    require(arity > 0)
    ngrams(affix(string)(arity))(arity)
  }

  def ngrams(string: String)(arity: Int): Seq[String] = {
    require(arity > 0)
    string.sliding(arity).toSeq
  }

  def bigramsWithAffixing(string: String): Seq[String] = ngramsWithAffixing(string)(2)
}
