package euler.a2

// for odd n = 2m-1
// right-top: i*i
// left-bottom i*i+1
// left-top, right-bottom: i*i-(i-1)

object Euler028 extends App {
  def f(m: Int) = {
    val n = 2 * m - 1
    n * (n + 1) * (2 * n + 1) / 3 + (m - 2) - n * (n - 1) / 2
  }

  import utils.Bench._

  pTimeIt(f(501))

}
