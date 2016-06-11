package euler.a3

object Euler040 extends App {
  def pow10(n: Int) = Array.fill(n)(10).product

  val bmax = 7
  val tmax = 6

  val iList = (for (i <- 0 until bmax) yield pow10(i) * (i + 1) * 9).scanLeft(0)((i, j) => i + j)
  val l = iList.zipWithIndex

  def getDigit(pindex: Int) = {
    val index = pindex - 1
    val base = l.takeWhile(e => e._1 <= index).last
    val remaining = index - base._1
    val start = pow10(base._2)
    val offset = remaining / (base._2 + 1)
    val pos = remaining - offset * (base._2 + 1)
    (start + offset).toString.charAt(pos).toInt - 48
  }

  def f(max: Int) = (for (i <- 0 until max) yield pow10(i)).map(i => getDigit(i)).product

  import utils.Bench._
  pTimeIt(f(tmax))


}
