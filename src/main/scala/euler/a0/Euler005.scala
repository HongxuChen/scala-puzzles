package euler.a0

import utils.Bench._

import scala.annotation.tailrec

object Euler005 extends App {

  @tailrec
  def gcd(a: Int, b: Int): Int = {
    (a, b) match {
      case (_, 0) => a
      case _ => gcd(b, a % b)
    }
  }

  def lcm(a: Int, b: Int) = {
    val r = if (a > b) gcd(a, b) else gcd(b, a)
    a / r * b
  }

  def f(max: Int) = (1 until max).foldRight(max)((res, ele) => lcm(res, ele))

  val max = 20
  pTimeIt(f(max))


}
