package euler.a3

object Euler031 extends App {

  def f1(n: Int): Int = {
    def inner(cur: Int, choices: Array[Int]): Int = choices.zipWithIndex.find(_._1 <= cur) match {
      case Some((value, index)) => {
        val remaining = cur - value
        val newChoices = choices.slice(index, choices.length)
        val lhs = if (remaining == 0) 1 else inner(remaining, newChoices)
        val rhs = inner(cur, newChoices.slice(1, newChoices.length))
        lhs + rhs
      }
      case None => 0
    }
    val coins = Array(200, 100, 50, 20, 10, 5, 2, 1)
    inner(n, coins)
  }

  def f2(n: Int): Int = {
    val coins = Array(1, 2, 5, 10, 20, 50, 100, 200)
    val ways = new Array[Int](n + 1)
    ways(0) = 1
    for (c <- coins; j <- c to n) ways(j) += ways(j - c)
    ways(n)
  }

  import utils.Bench._

  pTimeIt(f2(200))
  //  pTimeIt(f1(200), seconds = 1, chunk = 2, round = 1)
}
