package euler.a2

import scala.collection.mutable
import scala.util.control.Breaks._

object Euler024 extends App {

  def f(n: Int) = {
    val arr = Array.range(9, 0, -1).scanRight(1)(_ * _)
    val sb = new mutable.StringBuilder
    val lb = mutable.ListBuffer.range(0, 10)
    var remaining = n + 1
    breakable {
      for (i <- arr.indices) {
        val curFact = arr(i)
        val j = remaining / curFact
        remaining = remaining - curFact * j
        val removed = lb.remove(j)
        sb.append(removed)
        if (remaining == 0) break
      }
    }
    sb.append(lb.mkString)
    sb.toString
  }

  import utils.Bench._

  val num = 1000000
  pTimeIt(f(num))

}
