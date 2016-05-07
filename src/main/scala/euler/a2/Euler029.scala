package euler.a2

import scala.collection.mutable

object Euler029 extends App {
  def power(b: Int, n: Int) = (BigInt(1) /: Array.fill(n)(b)) (_ * _)

  def f(rangeA: (Int, Int), rangeB: (Int, Int)): Int = {
    val s = mutable.Set.empty[BigInt]
    for {
      a <- rangeA._1 to rangeA._2
      b <- rangeB._1 to rangeB._2
    } s += power(a, b)
    s.size
  }

  val rangeA = (2, 100)
  val rangeB = (2, 100)

  import utils.Bench._

  pTimeIt(f(rangeA, rangeB))

}
