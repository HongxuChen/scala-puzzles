package euler.a3

object Euler039 extends App {

  def encode[A](l: List[A], acc: List[(Int, A)], count: Int): List[(Int, A)] = l match {
    case Nil => acc
    case List(x) => (count + 1, x) :: acc
    case a :: b :: tail => if (a == b) encode(b :: tail, acc, count + 1) else encode(b :: tail, (count + 1, a) :: acc, 0)
  }

  def getSqrt(l: Int, h: Int, p: Int): Option[Int] = Stream.range(l, h).dropWhile(i => i * i != p).headOption

  def f(max: Int): Int = {
    val l = (for {
      i <- 3 to max / 3
      j <- i + 1 to max / 2
      prod = i * i + j * j
      sqrt = math.ceil(math.sqrt(prod.toDouble)).toInt
      if sqrt * sqrt == prod
      res = i + j + sqrt
      if res <= max
    } yield res).sorted
    encode(l.toList, Nil, 0).sortWith((e1, e2) => e1._1 > e2._1).head._2
  }

  import utils.Bench._
  pTimeIt(f(1000))

}
