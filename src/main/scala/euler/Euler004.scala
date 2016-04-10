package euler

import scala.util.control.Breaks._

object Euler004 extends App {
  @inline
  def isPalindrome1(v: Int) = {
    val s = v.toString
    s.reverse == s
  }

  @inline
  def isPalindrome2(v: Int) = {
    val s = v.toString
    val half = s.length / 2
    val (l, r) = (s.take(half), s.takeRight(half))
    l.reverse == r
  }

  def f_for(r: (Int, Int)) = {
    var max = 0
    for (i <- r._2 to r._1 by -1) {
      breakable {
        for (j <- i to r._1 by -1) {
          val v = i * j
          if (isPalindrome1(v)) {
            max = math.max(v, max)
            break
          }
        }
      }
    }
    max
  }

  def f_while(r: (Int, Int)) = {
    var max = 0
    var i = r._2
    while (i >= r._1) {
      breakable {
        var j = i
        while (j >= r._1) {
          val v = i * j
          if (isPalindrome1(v)) {
            max = math.max(v, max)
            break
          }
          j -= 1
        }
      }
      i -= 1
    }
    max
  }

  val range = (100, 999)
  val r1 = timeIt(f_while(range))
  val r2 = timeIt(f_for(range))
  println(s"$r1, $r2")

}
