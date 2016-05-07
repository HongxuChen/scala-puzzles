package euler.a2

object Euler030 extends App {

  def power(b: Int, n: Int) = (1 /: Array.fill(n)(b)) (_ * _)

  def m(n: Int, p: Int) = {
    def inner(cur: Int, sum: Int): Int = cur match {
      case 0 => sum
      case _ => val div = cur / 10; inner(div, sum + power(cur - div * 10, p))
    }
    n == inner(n, 0)
  }

  def f(p: Int): Int = (2 to power(9, p) * (p + 1)).filter(m(_, p)).sum

  import utils.Bench._

  pTimeIt(f(5), seconds = 1, chunk = 10, round = 2)

}
