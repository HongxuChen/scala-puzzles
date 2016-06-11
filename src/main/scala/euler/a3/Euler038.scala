package euler.a3

import scala.util.control.Breaks._


object Euler038 extends App {
  val (min, max) = (9, 9999)
  var res = 0
  val sortedSeq = (1 to 9).map(i => (i + 48).toChar).toSet

  def f = {
    for (i <- Stream.range(min, max)) {
      var remaining = sortedSeq
      var cur_res = ""
      breakable {
        for (j <- 1 to 9) {
          val ijStr = (i * j).toString
          val cur = ijStr.toSet
          if (cur.size != ijStr.length || !(cur subsetOf remaining)) break
          cur_res = cur_res + ijStr
          remaining = remaining &~ cur
          if (remaining.isEmpty) {
            res = i
            val cur_res_val = cur_res.toInt
            if (cur_res_val > res) {
              res = cur_res_val
            }
          }
        }
      }
    }
    res
  }

  import utils.Bench._

  pTimeIt(f)

}
