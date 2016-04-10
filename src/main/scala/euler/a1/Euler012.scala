package euler
package a1

import scala.collection.mutable

object Euler012 extends App {

  def f(n: Int): Int = {
    var count = 0
    Iterator.continually({
      count += 1
      count * (count + 1) / 2
    }).dropWhile(getNumOfDivisors(_) <= n).toStream.head
  }

  def getNumOfDivisors(num: Int): Int = {
    var m = mutable.Map.empty[Int, Int]
    var index = 2
    var current = num
    while (index <= current) {
      val numberator = current / index
      if (numberator * index == current) {
        m += index -> (m.getOrElse(index, 0) + 1)
        current = numberator
      } else {
        index += 1
      }
    }
    m.values.map(_ + 1).product
  }

  pTimeIt(f(500), 1, 10, 2)

}
