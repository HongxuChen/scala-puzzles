package euler.a3

object Euler037 extends App {

  def f(n: Int) = {
    val arr = Array.range(0, n + 1)
    arr(1) = 0
    for (i <- 2 to n; if arr(i) != 0) for (j <- i * 2 to n by i) arr(j) = 0
    def removedLeading(n: Int) = {
      def inner(n: Int, len: Int): (Int, Int) = if (n < 10) (n, len) else inner(n / 10, len * 10)
      val (leading, len) = inner(n, 1)
      n - leading * len
    }
    def left(num: Int): Boolean = arr(num) match {
      case 0 => false
      case _ => if (num < 10) true else left(removedLeading(num))
    }
    def right(num: Int): Boolean = arr(num) match {
      case 0 => false
      case _ => if (num < 10) true else right(num / 10)
    }
    (for (i <- 11 to n; if arr(i) != 0 && left(i) && right(i)) yield i).sum
  }

  import utils.Bench._

  val MAX = 1000000
  pTimeIt(f(MAX))

}
