package euler.a3

object Euler031 extends App {
  val coins = Array(200, 100, 50, 20, 10, 5, 2, 1)

  def f(n: Int): Int = {
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
    inner(n, coins)
  }

  import utils.Bench._

  pTimeIt(f(200), seconds = 1, chunk = 2, round = 1)
}
