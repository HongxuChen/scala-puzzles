package euler.a0

import utils.Bench._

object Euler001 extends App {
  def f(n: Int) = (for (i <- 1 until n; if i % 3 == 0 || i % 5 == 0) yield i).sum

  pTimeIt(f(1000))
}
