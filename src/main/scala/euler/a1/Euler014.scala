package euler
package a1

import scala.annotation.tailrec
import scala.collection.mutable

object Euler014 extends App {
  type KeyTy = Long
  type ValueTy = Int
  type MapTy = mutable.Map[KeyTy, ValueTy]
  type EleTy = (Int, ValueTy)

  def f1(n: Int): Int = {

    @inline
    def getChain(n: Int, m: MapTy): EleTy = {
      var current = n.toLong
      var buf = mutable.ArrayBuffer.empty[KeyTy]
      do {
        buf += current
        current = if (current % 2 == 0) current / 2 else 3 * current + 1
      } while (!m.contains(current))
      val knownCounter = m(current)
      val len = buf.length
      for (i <- buf.indices) {
        m += buf(i) -> (knownCounter + len - i)
      }
      n -> (knownCounter + len)
    }

    val m = mutable.Map[KeyTy, ValueTy](1L -> 1)
    var res: (Int, ValueTy) = (0, 0)
    for (i <- 2 to n) {
      if (!m.contains(i)) {
        val cur = getChain(i, m)
        if (res._2 < cur._2) {
          res = cur
        }
      }
    }
    res._1
  }

  def f2(n: Int): Int = {
    @tailrec
    def from(n: Long, c: Int = 0): Int = {
      n match {
        case 1 => c + 1
        case _ => from(if (n % 2 == 0) n / 2 else 3 * n + 1, c + 1)
      }
    }

    (1 until n).view.map(n => (n, from(n)))
      .reduceLeft((a, b) => if (a._2 > b._2) a else b)._1
  }

  val n = 1000000
  //  pTimeIt(f1(n), seconds = 1, round = 2, chunk = 2)
  pTimeIt(f2(n), seconds = 1, round = 2, chunk = 2)

}
