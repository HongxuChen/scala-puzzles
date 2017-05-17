package spoj


object _002_PRIME1 extends App {

  import scala.io.StdIn._

  def isPrime(num: Int): Boolean = (2 until num).view.takeWhile(i => i * i <= num).forall(i => i * (num / i) < num)

  def sieveAll(m: Int, n: Int): Unit = {
    val primeArray = Array.fill(n - m + 1)(true)

    val basicPrimes = simpleSieve(math.sqrt(n.toDouble).toInt)
    //    for(p<- basicPrimes)sieve(m, n, p, primeArray)
    basicPrimes.foreach(p => sieve(m, n, p, primeArray))
    if (m == 1) {
      primeArray(0) = false
    }
    for (j <- primeArray.indices; if primeArray(j)) println(j + m)
    //    Array.tabulate(primeArray.length) { i => {
    //      if (primeArray(i)) {
    //        println(i + m)
    //      }
    //    }
    //    }
  }

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

  (1 to readInt()).foreach(_ => {
    val ss = readLine().split("\\s+").map(_.toInt)
    val (m, n) = (ss.head, ss.last)
    sieveAll(m, n)
  })
}
