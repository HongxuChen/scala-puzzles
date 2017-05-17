package euler.a0

import utils.Bench._

import scala.util.control.Breaks._

object Euler009 extends App {

  def f(m: Int): Int = {
    var res = 0
    for (a <- 1 to m / 3) {
      breakable {
        val d = m * (m - 2 * a)
        val n = 2 * (m - a)
        val b = d / n
        if (b * n == d) {
          val cSquare = a * a + b * b
          val c = math.sqrt(cSquare.toDouble).toInt
          if (c * c == cSquare) {
            res = a * b * c
            break
          }
        }
      }
    }
    res
  }

  pTimeIt(f(1000))

}
