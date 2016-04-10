package euler

object Euler001 extends App {
  println((for (i <- 1 until 1000; if i % 3 == 0 || i % 5 == 0) yield i).sum)
}
