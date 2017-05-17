package reddit

import scala.io.{Source, StdIn}

object _246_LetterSplits extends App {

  val realWords = Source.fromURL(getClass.getResource("enable1.txt")).getLines().map(_.toUpperCase()).toSet
  val m = (1 to 26).map(i => (i.toString, (i + 'A' - 1).toChar)).toMap
  val input = StdIn.readLine()
  val start = System.currentTimeMillis()

  def isRealWord(s: String, min: Int = 5): Boolean = {
    s.length match {
      case 0 => true
      case _ => (min to s.length).exists(i => {
        val (l, r) = s.splitAt(i)
        realWords.contains(l) && isRealWord(r)
      })
    }
  }

  def decode(input: String, output: String): List[String] =
    input.length match {
      case 0 => List(output).filter(s => isRealWord(s))
      case _ =>
        List(1, 2).flatMap(num => {
          if (input.length >= num && m.contains(input.substring(0, num))) {
            val (h, t) = input.splitAt(num)
            decode(t, output + m(h).toString)
          } else {
            Nil
          }
        })
    }

  def decode(input: String): List[String] = decode(input, "")

  decode(input).foreach(println)
  println(s"takes ${System.currentTimeMillis() - start}ms")

}
