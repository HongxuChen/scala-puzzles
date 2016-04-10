package euler.a0

import euler._

import scala.util.control.Breaks._

object Euler007 extends App {

  // stream always does worse than Iterator.continually or Range(...).view
  // inline doesn't take much effect, perhaps already inlined

  def isPrime(n: Int) = Range(2, n).view.takeWhile(i => i * i <= n).forall(i => i * (n / i) != n)

  def f1(num: Int) = {
    var count = 1
    var order = 0
    var prime = 0
    Iterator.continually({
      count += 1
      count
    }).takeWhile(_ => order < num).withFilter(isPrime).foreach(i => {
      order += 1
      prime = i
    })
    prime
  }

  case class EndExcept(i: Int) extends Throwable

  def f2(num: Int) = {
    var count = 1
    var order = 0
    var prime = 0
    try {
      Iterator.continually({
        count += 1
        count
      }).withFilter(isPrime).foreach(i => {
        breakable {
          if (order < num) {
            order += 1
          } else {
            throw new EndExcept(i)
          }
        }
      })
    } catch {
      case e: EndExcept => prime = e.i
    }
    prime
  }

  val max = 10001
  pTimeIt(f1(max), round = 3)
  pTimeIt(f2(max), round = 3)
}
