package euler.a4

object Euler043 extends App {

  val odds = Set(0, 2, 4, 6, 8)

  def getDigit(x: Int, y: Int, z: Int) = 100 * x + 10 * y + z

  def f = {
    val candidates = (for {
      l <- (0 to 9).permutations
      if l(5) == 5 && (l(5) + l(7) - l(6) == 0 || l(5) + l(7) - l(6) == 11) && odds.contains(l(3)) && (l(2) + l(3) + l(4)) % 3 == 0
      if getDigit(l(4), l(5), l(6)) % 7 == 0 && getDigit(l(6), l(7), l(8)) % 13 == 0 && getDigit(l(7), l(8), l(9)) % 17 == 0 // 17
    } yield l.foldLeft(0L)((acc, cur) => 10 * acc + cur)).toList
    candidates.sum
  }

  import utils.Bench._

  pTimeIt(f, chunk = 2)


}
