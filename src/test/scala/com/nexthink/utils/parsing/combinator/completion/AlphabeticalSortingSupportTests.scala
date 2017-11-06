package com.nexthink.utils.parsing.combinator.completion

import java.util.Random

import org.junit.{Assert, Test}

class AlphabeticalSortingSupportTests {
  object testParser extends AlphabeticalSortingSupport

  private val rand       = new Random(75483758457L)
  private val fixedList  = (0 to 10000).reverse.flatMap(i => List("a" + i, "a" + i))
  private val randomList = (0 to 10000).reverse.flatMap(i => List("a" + rand.nextInt(), "a" + rand.nextInt()))

  private def testList(list: Seq[String], expected: List[(String, Int)]): Unit = {
    val completions: CompletionTypes#CompletionSet = testParser.alphabeticallySortedCompletions(list, 15)

    val map = completions.completions.map { case (k, v) => (k.toString, v.score) }.toList.sortBy(_._2).reverse

    Assert.assertEquals(expected, map)
  }

  @Test def fixed(): Unit = {
    testList(
      fixedList,
      List(
        "a0"     -> 15,
        "a1"     -> 14,
        "a10"    -> 13,
        "a100"   -> 12,
        "a1000"  -> 11,
        "a10000" -> 10,
        "a1001"  -> 9,
        "a1002"  -> 8,
        "a1003"  -> 7,
        "a1004"  -> 6,
        "a1005"  -> 5,
        "a1006"  -> 4,
        "a1007"  -> 3,
        "a1008"  -> 2,
        "a1009"  -> 1
      )
    )
  }

  @Test def random(): Unit = {
    testList(
      randomList,
      List(
        "a-1000161555" -> 15,
        "a-1000182114" -> 14,
        "a-100035620"  -> 13,
        "a-1000377965" -> 12,
        "a-1000818147" -> 11,
        "a-1000969668" -> 10,
        "a-1001519856" -> 9,
        "a-1001584554" -> 8,
        "a-1001626909" -> 7,
        "a-1001678742" -> 6,
        "a-1001772693" -> 5,
        "a-1001778706" -> 4,
        "a-1001842806" -> 3,
        "a-1001929237" -> 2,
        "a-1002108133" -> 1
      )
    )
  }

}
