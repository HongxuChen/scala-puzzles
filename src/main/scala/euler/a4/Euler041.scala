package euler.a4

object Euler041 extends App {

  def isPrime(n: Int) = Range(2, n).view.takeWhile(i => i * i <= n).forall(i => n / i * i != n)

  def f(n: Int) = {
    (for {
      p <- (1 to n).permutations
      res = p.reduceLeft((acc, cur) => acc * 10 + cur)
      if isPrime(res)
    } yield res).max
  }

  import utils.Bench._

  pTimeIt(f(7))

}
