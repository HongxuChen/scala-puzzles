package euler.a0

import utils.Bench._

object Euler005 extends App {

  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  def lcm(a: Int, b: Int) = a / gcd(a, b) * b

  def f(max: Int) = (1 until max).foldRight(max)((res, ele) => lcm(res, ele))

  val max = 20
  pTimeIt(f(max))
  
}
