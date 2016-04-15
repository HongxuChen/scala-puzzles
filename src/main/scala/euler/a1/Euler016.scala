package euler.a1

import utils.Bench._

import scala.math.BigInt

object Euler016 extends App {

  val zero = BigInt(0)

  def f(num: BigInt, exp: BigInt): BigInt = {
    exp match {
      case `zero` => 1
      case _ =>
        val exp1 = exp / 2
        if (exp1 * 2 == exp) {
          f(num, exp1) * f(num, exp1)
        } else {
          f(num, exp1) * f(num, exp1) * num
        }
    }
  }

  pTimeIt(f(2, 1000).toString.map(_.toInt - 48).sum)
  // 1366

}
