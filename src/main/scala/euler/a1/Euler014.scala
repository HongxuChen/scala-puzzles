package euler
package a1

import scala.collection.mutable

object Euler014 extends App {

  def getChain(n: Int) = {
    var current = n
    var counter = 1
    var buf = mutable.ArrayBuffer(current)
    while (current != 1) {
      current = if (current % 2 == 0) current / 2 else 3 * current + 1
      buf += current
      counter += 1
    }
    buf
  }

  def f(n: Int) = {
    val counters = Array.ofDim[Int](n)
    for (i <- 1 to n) {
      if (counters(i - 1) == 0) {
        val buf = getChain(i)
        //        println(s"$i, $buf")
        for (j <- buf.indices) {
          val number = buf(j)
          if (number < counters.length) {
            counters(number - 1) = buf.length - j
          }
        }
      }
    }

  }

  println(getChain(6))
  println(getChain(16))
  val n = 1000000
  //  val n = 20
  println(f(n))

}
