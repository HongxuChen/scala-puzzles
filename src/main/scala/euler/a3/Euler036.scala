package euler.a3

object Euler036 extends App {
  def f1(n: Int): Int = (for (i <- 1 until n; ds = i.toString; if ds.reverse == ds; bs = Integer.toBinaryString(i); if bs.reverse == bs) yield i).sum

  def reverse(n: Int): Int = {
    def inner(n: Int, acc: Int): Int = n match {
      case 0 => acc
      case _ => val div = n / 10; inner(div, acc * 10 + n - div * 10)
    }
    inner(n, 0)
  }

  def f2(n: Int): Int = (for (i <- 1 until n; if i == reverse(i); bs = Integer.toBinaryString(i); if bs.reverse == bs) yield i).sum

  import utils.Bench._

  pTimeIt(f2(1000000))

}
