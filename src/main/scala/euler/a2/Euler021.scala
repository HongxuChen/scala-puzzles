package euler.a2

import scala.collection.mutable

object Euler021 extends App {

  def primeFactorMultiplicity(num: Int) = {
    val m = mutable.Map.empty[Int, Int]
    var dividend = num
    var start = 2
    while (start <= dividend) {
      val quotient = dividend / start
      if (quotient * start == dividend) {
        dividend = quotient
        m += start -> (m.getOrElse(start, 0) + 1)
      } else {
        start += 1
      }
    }
    m
  }

  def power(b: Int, n: Int) = Array.fill(n)(b).product

  def sumOfDivisors(num: Int) = {
    val m = primeFactorMultiplicity(num)
    (for ((d, c) <- m) yield (power(d, c + 1) - 1) / (d - 1)).product - num
  }

  def f(n: Int) = {
    var sum = 0
    val array = Array.fill(n + 1)(0)
    for (i <- 2 to n) {
      if (array(i) == 0) {
        val divSum = sumOfDivisors(i)
        if (divSum != i && sumOfDivisors(divSum) == i) {
          sum += (divSum + i)
          array(divSum) = 1
        }
      }
    }
    sum
  }

  val myNum = 10000

  import utils.Bench._

  pTimeIt(f(myNum))

}
