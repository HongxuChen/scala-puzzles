package euler.a4

object Euler048 extends App {

  def f(max: Int) = {
    val modular = 10000000000L
    def moduleExpr(i: Int) = List.fill(i)(i).foldLeft(1L)((j: Long, k: Int) => j * k % modular)
    val l = for (i <- 1 to max) yield moduleExpr(i)
    l.foldLeft(0L)((i: Long, j: Long) => (i + j) % modular)
  }

  import utils.Bench._

  pTimeIt(f(1000))

}
