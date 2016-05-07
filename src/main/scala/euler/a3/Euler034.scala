package euler.a3

object Euler034 extends App {

  def f = {
    val facts = Array.range(1, 10).scanLeft(1)(_ * _)
    def factSum(n: Int) = {
      def inner(n: Int, acc: Int): Int = n match {
        case 0 => acc
        case _ => val div = n / 10; inner(div, acc + facts(n - div * 10))
      }
      inner(n, 0)
    }
    (for (i <- 10 until facts(9) * 7; if factSum(i) == i) yield i).sum
  }

  import utils.Bench._

  pTimeIt(f)
}
