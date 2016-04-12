package euler.a0

import utils.Bench._

object Euler010 extends App {
  def do1(a: Array[Int], offSet: Int): Unit = {
    a.foreach {
      case 0 =>
      case i => {
        for (j <- 2 * i - offSet until n - offSet by i) {
          a(j) = 0
        }
      }
    }
  }

  def do2(a: Array[Int], offSet: Int): Unit = {
    for (i <- a) {
      i match {
        case 0 =>
        case _ => {
          for (j <- 2 * i - offSet until n - offSet by i) {
            a(j) = 0
          }
        }
      }
    }
  }

  def f(n: Int, offSet: Int = 2) = {
    val a = Array.range(offSet, n)
    do2(a, offSet)
    a.map(_.toLong).sum
  }

  val n = 2000000
  pTimeIt(f(n))
}
