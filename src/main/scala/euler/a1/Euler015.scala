package euler.a1

import utils.Bench._

import scala.collection.mutable

/**
  * @todo write my own memo version
  */
object Euler015 extends App {
  type MyTy = (Int, Int)

  // Explaination: there are 2 choices: down OR right, and totally 2*n steps to finish
  // down(0) takes n steps and right(1) n steps, all valid movements contain n 0s and n 1s
  // therefore 2*n chooses n

  def f1(n: Int): Long = {
    val cache = mutable.Map.empty[MyTy, Long]
    def inner(res: MyTy): Long = {
      res match {
        case (0, _) | (_, 0) => 1L
        case (i, j) => {
          if (cache.contains(res)) {
            cache(res)
          } else {
            val current = inner((i, j - 1)) + inner((i - 1, j))
            cache += res -> current
            current
          }
        }
      }
    }
    inner((n, n))
  }

  def f2(n: Int): BigInt = {
    val r = 1 to n
    r.foldLeft(BigInt(1))((p, i) => p * (i + n)) / r.foldLeft(BigInt(1))(_ * _)
  }

  val n = 20
  pTimeIt(f1(n))
  pTimeIt(f2(n))

}
