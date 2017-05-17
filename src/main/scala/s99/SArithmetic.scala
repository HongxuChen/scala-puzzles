package s99

import scala.collection.mutable

object SArithmetic {

  implicit class ArithmeticNumeric(num: Int) {
    // 31
    def isPrime: Boolean = num match {
      case _ if num <= 1 => false
      case _ => Range(2, num).view.takeWhile(i => i * i <= num).forall(i => num / i * i < num)
    }

    // 33
    def isCoprimeTo(that: Int): Boolean = gcd(num, that) == 1

    // 34
    def totient: Int = (1 until num).count(isCoprimeTo)

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
    def totientImproved: Int = {
      def power(n: Int, p: Int) = List.fill(p)(n).product
      (primeFactorMultiplicity map { case (factor, count) => (factor - 1) * power(factor, count - 1) }).product
    }

    // 40
    def goldbach: (Int, Int) = {
      val first = Stream.range(3, num).dropWhile(i => !i.isPrime || !(num - i).isPrime).head
      (first, num - first)
    }

  }

  // implicit class

  // 32
  def gcd(a: Int, b: Int): Int = (a, b) match {
    // a>b for most of the cases
    case (_, 0) => a
    case (_, _) => gcd(b, a % b)
  }

  // 38print
  def compareTotient(num: Int): ((Int, Long), (Int, Long)) = {
    def timed[A](thunk: => A): (A, Long) = {
      val startTime = System.nanoTime()
      val res = thunk
      val duration = System.nanoTime() - startTime
      (res, duration)
    }
    (timed(num.totient), timed(num.totientImproved))
  }

  // 39

  def listPrimesinRange(m: Int, n: Int): List[Int] = {
    def sieve(m: Int, n: Int, step: Int, primeArray: Array[Boolean]): Unit = {
      val start = {
        if (m <= step) {
          2 * step
        } else {
          val pre = (m / step) * step
          if (pre == m) {
            pre
          } else {
            pre + step
          }
        }
      }
      Range(start, n + 1, step).foreach(i => primeArray(i - m) = false)
    }

    def simpleSieve(num: Int): IndexedSeq[Int] = {
      val basicPrimes = Array.fill(num - 1)(true)
      for {
        i <- 2 to num
        if basicPrimes(i - 2)
        j <- i * 2 to num by i
      } basicPrimes(j - 2) = false
      for {
        j <- basicPrimes.indices
        if basicPrimes(j)
      } yield j + 2
    }
    val primeArray = Array.fill(n - m + 1)(true)

    val basicPrimes = simpleSieve(math.sqrt(n.toDouble).toInt)
    //    for(p<- basicPrimes)sieve(m, n, p, primeArray)
    basicPrimes.foreach(p => sieve(m, n, p, primeArray))
    if (m == 1) {
      primeArray(0) = false
    }
    val res = for (j <- primeArray.indices; if primeArray(j)) yield j + m
    res.toList
  }

  // 41
  def printGoldbachList(m: Int, n: Int): List[(Int, (Int, Int))] = {
    for (i <- List.range(m, n + 1); if i % 2 == 0) yield (i, i.goldbach)
  }

  def printGoldbachListLimited(m: Int, n: Int, threshold: Int): List[(Int, (Int, Int))] = {
    for (i <- List.range(math.max(m, threshold), n + 1); if i % 2 == 0; v = i.goldbach; if v._1 > threshold) yield (i, v)
  }
}