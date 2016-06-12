package euler.a4

import scala.io.Source

object Euler042 extends App with utils.Common {
  val file = curDir + "p042_words.txt"

  def f(file: String) = {
    val words = Source.fromFile(file).getLines().mkString.replaceAll("\"", "").split(',')
    val valueArray = (for (w <- words) yield w.toList.map(_ - 'A' + 1).sum).sorted
    val max = math.ceil(math.sqrt(valueArray.last * 2)).toInt
    val s = (for (i <- 1 to max) yield i * (i + 1) / 2).toSet
    valueArray.count(v => s.contains(v))
  }

  import utils.Bench._
  pTimeIt(f(file))

}
