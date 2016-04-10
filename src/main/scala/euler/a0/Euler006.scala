package euler.a0

import euler._

object Euler006 extends App {
  def f1(n: Int): Long = {
    val numbers = 1 to n
    def square(n: Int) = n.toLong * n
    square(numbers.sum) - numbers.map(square).sum
  }

  def f2(n: Int): Long = (n.toLong - 1) * n / 2 * (n + 1) / 3 * (3 * n + 2) / 2

  pTimeIt(f2(10000), 3, 100, 2)
  pTimeIt(f1(10000), 3, 100, 2)

}
