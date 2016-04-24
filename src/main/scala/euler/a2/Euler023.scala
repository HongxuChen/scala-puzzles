package euler.a2

import scala.collection.mutable

object Euler023 extends App {

  /**
    * ADDITIONAL TUNING NOTES:
    * True limit is 20162
    * A multiple of an abundant number is abundant
    * A multiple (>1) of a perfect number is abundant
    * All even numbers above 46 can be written as a sum of two abundant numbers
    * for x>=12, x=6*i => x is redundant; however it doesn't work better
    */

  val MIN = 12

  def r0(n: Int): IndexedSeq[Int] = {
    import Euler021._
    for {
      i <- MIN to n - MIN
      if i < sumOfDivisors(i)
    } yield i
  }

  def r1(n: Int): IndexedSeq[Int] = {
    val sumOfDivs = Array.fill(n + 1)(0)
    for {
      i <- 1 to n / 2
      j <- i * 2 to n by i
    } sumOfDivs(j) += i
    for {
      i <- MIN to n - MIN
      if i < sumOfDivs(i)
    } yield i
  }

  def r2(n: Int): IndexedSeq[Int] = {
    val sumOfDivs = Array.fill(n + 1)(0)
    var i = 1
    while (i <= n / 2) {
      var j = i + i
      while (j <= n) {
        sumOfDivs(j) += i
        j += i
      }
      i += 1
    }
    val arr = mutable.ArrayBuffer.empty[Int]
    var k = MIN
    while (k <= n - MIN) {
      if (k < sumOfDivs(k)) arr += k
      k += 1
    }
    arr
  }

  // set has poorer performance
  def s0(n: Int, redundants: IndexedSeq[Int]): Int = {
    val arr = Array.fill(n + 1)(0)
    for {
      i <- redundants.indices
      j <- i until redundants.length
      res = redundants(i) + redundants(j)
      if res <= n
    } {
      arr(res) = res
    }
    n * (n + 1) / 2 - arr.sum
  }

  def s1(n: Int, redundants: IndexedSeq[Int]): Int = {
    val arr = Array.fill(n + 1)(0)
    var i = 0
    val length = redundants.length
    while (i < length) {
      var j = i
      while (j < length) {
        val res = redundants(i) + redundants(j)
        if (res <= n) arr(res) = res
        j += 1
      }
      i += 1
    }
    n * (n + 1) / 2 - arr.sum
  }

  def f[A](n: Int, r: Int => A, s: (Int, A) => Int) = s(n, r(n))

  val num = 28123

  import utils.Bench._


  for {
    r <- Array(r2 _, r1 _, r0 _)
    s <- Array(s1 _, s0 _)
  } pTimeIt(f(num, r, s), 1, 1, 2)

}
