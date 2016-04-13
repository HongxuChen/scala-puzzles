package s99

import scala.collection.mutable

object SArithmetic {

  implicit class ArithmeticNumeric(num: Int) {
    // 31
    def isPrime = num match {
      case _ if num <= 1 => false
      case _ => Range(2, num).view.takeWhile(i => i * i <= num).forall(i => num / i * i < num)
    }

    // 33
    def isCoprimeTo(that: Int) = gcd(num, that) == 1

    // 34
    def totient = (1 until num).count(isCoprimeTo(_))

    // 35
    def primeFactors: List[Int] = {
      val lb = mutable.ListBuffer.empty[Int]
      var dividend = num
      var start = 2
      while (start <= dividend) {
        val quotient = dividend / start
        if (quotient * start == dividend) {
          dividend = quotient
          lb += start
        } else {
          start += 1
        }
      }
      lb.toList
    }

    // 36
    def primeFactorMultiplicity: Map[Int, Int] = {
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
      // as required
      m.toMap
    }

    def primeFactorMultiplicity2: List[(Int, Int)] = primeFactorMultiplicity.toList.sortBy(_._1)

    // 37
    def totientImproved = {
      def power(n: Int, p: Int) = List.fill(p)(n).product
      (primeFactorMultiplicity map { case (factor, count) => (factor - 1) * power(factor, count - 1) }).product
    }

  }

  // 32
  def gcd(a: Int, b: Int): Int = (a, b) match {
    // a>b for most of the cases
    case (_, 0) => a
    case (_, _) => gcd(b, a % b)
  }

  // 38
  def compareTotient(num:Int) = {
    def timed[A](thunk: => A): (A, Long) = {
      val startTime = System.nanoTime()
      val res = thunk
      val duration = System.nanoTime() - startTime
      (res, duration)
    }
    (timed(num.totient), timed(num.totientImproved))
  }



}
