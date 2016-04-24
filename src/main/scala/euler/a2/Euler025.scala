package euler.a2

object Euler025 extends App {

  def f(digit: Int): Int = {
    val ub = BigInt("1" + "0" * (digit - 1))
    lazy val fibs: Stream[BigInt] = BigInt(0) #:: BigInt(1) #:: fibs.zip(fibs.tail).map(n => n._1 + n._2)
    fibs.takeWhile(_ < ub).length
  }

  import utils.Bench._

  val num = 1000
  pTimeIt(f(num))

}
