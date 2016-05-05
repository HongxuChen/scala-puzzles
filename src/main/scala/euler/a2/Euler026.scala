package euler.a2

import scala.collection.mutable

// http://mathworld.wolfram.com/DecimalExpansion.html

object Euler026 extends App {


  def coPrime10(n: Int) = {
    def coPrime(n: Int, b: Int): Int = if (n % b == 0) coPrime(n / b, b) else n
    coPrime(coPrime(n, 2), 5)
  }

  def kPower(n: Int) = {
    var k = 1
    var res = BigInt(10)
    while ((res - 1) % n != 0) {
      res *= 10
      k += 1
    }
    k
  }

  def f(n: Int) = {
    var maxPair = 1 -> 0
    val m = mutable.Map(maxPair)
    for (i <- 1 to n) {
      val p = coPrime10(i)
      if (!m.contains(p)) {
        val pVal = kPower(p)
        if (maxPair._2 < pVal) {
          maxPair = i -> pVal
        }
      }
    }
    maxPair._1
  }

  import utils.Bench._

  pTimeIt(f(1000))


}
