package euler.a1

import utils.Bench._

object Euler017 extends App {

  val m1 = Map(
    0 -> "",
    1 -> "one",
    2 -> "two",
    3 -> "three",
    4 -> "four",
    5 -> "five",
    6 -> "six",
    7 -> "seven",
    8 -> "eight",
    9 -> "nine"
  ).map { case (n, str) => n -> str.length }

  val m2 = Map(
    10 -> "ten",
    11 -> "eleven",
    12 -> "twelve",
    13 -> "thirteen",
    14 -> "fourteen",
    15 -> "fifteen",
    16 -> "sixteen",
    17 -> "seventeen",
    18 -> "eighteen",
    19 -> "nineteen"
  ).map { case (n, str) => n -> str.length }

  val c = Map(
    20 -> "twenty",
    30 -> "thirty",
    40 -> "forty",
    50 -> "fifty",
    60 -> "sixty",
    70 -> "seventy",
    80 -> "eighty",
    90 -> "ninety"
  ).map { case (n, str) => n -> str.length }

  def f: Int = {
    val n1 = m1.values.sum // 1-9
    val n21 = m2.values.sum // 10-19
    val n22 = 10 * c.values.sum + c.size * n1 // 20-99
    val s2 = n1 + n21 + n22 // 1-99
    val HUNDRED = 7
    val AND = 3
    val n3 = n1 * 100 + 9 * 100 * HUNDRED + 9 * 99 * AND + 9 * s2
    s2 + n3
  }

  pTimeIt(f + 11)


}
