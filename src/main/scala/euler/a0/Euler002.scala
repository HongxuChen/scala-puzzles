package euler.a0

import utils.Bench._

object Euler002 {

  def f1(max: Int): Long = {
    var sum = 0L
    var (t1, t2) = (1, 2)
    while (t1 <= max) {
      if (t1 % 2 == 0) {
        sum += t1
      }
      val tmp = t2
      t2 = t1 + t2
      t1 = tmp
    }
    sum
  }

  def f2(max: Int): Long = {
    lazy val fs: Stream[Long] = 0L #:: fs.scanLeft(1L)(_ + _)
    fs.view.takeWhile(_ <= max).filter(_ % 2 == 0).sum
  }

  def main(args: Array[String]): Unit = {
    val max = 40000000
    pTimeIt(f1(max))
    pTimeIt(f2(max))
  }

}
