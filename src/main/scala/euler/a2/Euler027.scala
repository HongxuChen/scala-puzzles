package euler.a2

object Euler027 extends App {

  val rangeA = (-999, 999)
  val rangeB = (-999, 999)

  def isPrime(n: Int) = if (n < 0) false else (2 to n).takeWhile(i => i * i <= n).forall(i => n / i * i != n)

  def numOfPrimes(a: Int, b: Int): Int = Stream.from(0).takeWhile(i => isPrime(i * i + a * i + b)).size

  def f(rangeA: (Int, Int), rangeB: (Int, Int)) = {
    val l = for {
      b <- rangeB._1 to rangeB._2
      if isPrime(b)
      a <- rangeA._1 to rangeA._2 by 2
    } yield numOfPrimes(a, b) -> a * b
    l.sortBy(-_._1).head._2
  }

  import utils.Bench.pTimeIt

  pTimeIt(f(rangeA, rangeB))

}
