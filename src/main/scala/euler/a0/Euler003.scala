package euler.a0

import euler._

import scala.collection.mutable

object Euler003 extends App {
  def f1(num: Long) = {
    var number = num
    var cursor = 2
    var result = number / cursor
    var s = mutable.Set.empty[Long]
    var ub = math.sqrt(number).toLong
    while (cursor <= ub) {
      result = number / cursor
      if (result * cursor == number) {
        number = result
        s += cursor
        ub = math.sqrt(number).toLong
      } else {
        cursor += 1
      }
    }
    s += result
    s.max
  }

  def f2(num: Long): Long = {
    def f2_inner(num: Long, divisor: Long): Set[Long] = {
      num % divisor match {
        case 0 => f2_inner(num / divisor, divisor)
        case _ => {
          if (divisor > math.sqrt(num)) Set(num)
          else f2_inner(num, divisor + 1)
        }
      }
    }
    f2_inner(num, 2).max
  }


  val num = 600851475143L
  pTimeIt(f1(num))
  pTimeIt(f2(num))

}
