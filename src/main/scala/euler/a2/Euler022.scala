package euler.a2

import scala.io.Source

object Euler022 extends App with utils.Common {

  def f(file: String) = {
    val words = Source.fromFile(file).getLines().mkString.replaceAll("\"", "").split(",").sorted
    (for ((w, i) <- words.zipWithIndex) yield w.map(_.toInt - 64).sum * (i + 1)).sum
  }

  val file = curDir + "p022_names.txt"

  import utils.Bench._

  pTimeIt(f(file))


}
