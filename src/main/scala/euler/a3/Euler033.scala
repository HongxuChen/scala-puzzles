package euler.a3

object Euler033 extends App {

  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  def f = {
    val l = for {
      i <- 11 to 99; if i % 10 != 0; is = i.toString
      j <- i + 1 to 99; if j % 10 != 0; js = j.toString
      iis <- is; if js.contains(iis)
      di = is.replaceFirst(iis.toString, "").head.asDigit
      dj = js.replaceFirst(iis.toString, "").head.asDigit
      if di * j == dj * i
    } yield (di, dj)
    val res = ((1, 1) /: l) ((x, y) => (x._1 * y._1, x._2 * y._2))
    res._2 / gcd(res._1, res._2)
  }

  println(f)

}
