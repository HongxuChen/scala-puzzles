package euler.a1

object Euler019 extends App {

  val months = 1 to 12
  val days = Seq(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

  val md = months.zip(days.map(_ % 7)).toMap

  @inline
  def leapYearBonus(year: Int) = if (year % 400 == 0 || (year % 4 == 0 && year % 100 != 0)) 1 else 0

  def countSundays(startYear: Int, endYear: Int, offInit: Int) = {
    var offset = offInit
    var count = if (offset == 0) 1 else 0
    var curYear = startYear
    var curMonth = 1
    while (curYear <= endYear) {
      val increased = md(curMonth)
      curMonth match {
        case 12 => {
          offset = (offset + increased) % 7
          curYear += 1
          curMonth = 1
        }
        case 2 => {
          offset = (offset + increased + leapYearBonus(curYear)) % 7
          curMonth += 1
        }
        case _ => {
          offset = (offset + increased) % 7
          curMonth += 1
        }
      }
      if (offset == 0) count += 1
    }
    count
  }

  val (startYear, endYear) = (1901, 2000)
  var offSet = (6 + 365 + leapYearBonus(1900)) % 7

  import utils.Bench._

  pTimeIt(countSundays(startYear, endYear, offSet))


}
