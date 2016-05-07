package euler.a3

object Euler035 extends App {

  def isCircularPrime(num: Int, nLength: Int, dividend: Int, arr: Array[Int]): Boolean = {
    def rotate(pre: Int) = {
      val divisor = pre / dividend
      (pre - divisor * dividend) * 10 + divisor
    }
    //noinspection ScalaUnnecessaryParentheses
    (new Array[Int](nLength - 1)).scanLeft(num)((pre, _) => rotate(pre)).forall(num => arr(num) != 0)
  }

  def f(n: Int): Int = {
    val lengthArr = Array.fill(math.log10(n).toInt + 1)(10).scanLeft(1)(_ * _)
    val arr = Array.range(0, n + 1)
    for (i <- 2 to n; if arr(i) != 0) for (j <- i * 2 to n by i) arr(j) = 0
    var counter = 0
    var length = 1
    var dividend = 1
    for (i <- 2 to n; if arr(i) != 0) {
      if (i >= lengthArr(length)) {
        length += 1
        dividend *= 10
      }
      if (isCircularPrime(i, length, dividend, arr)) counter += 1
    }
    counter
  }

  import utils.Bench._

  pTimeIt(f(1000000))

}
