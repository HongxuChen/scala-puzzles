package euler.a1

object Euler020 extends App {


  def fact(n: Int): BigInt = {
    def inner(n: Int, acc: BigInt): BigInt = n match {
      case 1 => acc
      case _ => inner(n - 1, acc * n)
    }
    inner(n, 1)
  }

  import utils.Bench._
  val num = 100
//  println(fact(num))
  pTimeIt(fact(num).toString.map(_.toLong - 48L).sum)

}
