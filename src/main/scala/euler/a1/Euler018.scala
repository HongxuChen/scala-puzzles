package euler.a1


import scala.io.Source

object Euler018 {
  val triangleStr = "75\n95 64\n17 47 82\n18 35 87 10\n20 04 82 47 65\n19 01 23 75 03 34\n88 02 77 73 07 63 67\n99 65 04 28 06 16 70 92\n41 41 26 56 83 40 80 70 33\n41 48 72 33 47 32 37 16 94 29\n53 71 44 65 25 43 91 52 97 51 14\n70 11 33 28 77 73 17 78 39 68 17 57\n91 71 52 38 17 14 91 43 58 50 27 29 48\n63 66 04 68 89 53 67 30 73 16 69 87 40 31\n04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"
  val lineNum = triangleStr.count(_ == '\n') + 1
  val triangle = triangleStr.split("\\s+").map(_.toInt)
}

object Euler067 {

  import java.io.File

  private val file = {
    val src = s"src${File.separator}main${File.separator}scala"
    val dir = getClass.getName.replaceAll("\\.\\w+\\$$", "").replace('.', File.separatorChar)
    src + File.separator + dir + File.separator + "p067_triangle.txt"
  }

  private val lines = Source.fromFile(file).getLines().toArray
  val triangle = for {
    line <- lines
    num <- line.split("\\s+").map(_.toInt)
  } yield {
    num
  }
  val lineNum = lines.length
}

object Euler_TriangleSum extends App {

  def sum1(height: Int, array: Array[Int]): Int = {
    def inner(i: Int, j: Int): Int = {
      val index = i * (i + 1) / 2 + j
      i + 1 match {
        case `height` => array(index)
        case _ => {
          val max = math.max(inner(i + 1, j), inner(i + 1, j + 1))
          array(index) + max
        }
      }
    }
    inner(0, 0)
  }

  def sum2(height: Int, original: Array[Int]): Int = {
    val array = original.clone // keep original untouched
    for {
      i <- height - 2 to 0 by -1
      j <- 0 to i
    } {
      val index = i * (i + 1) / 2 + j
      val indexBelowStart = (i + 1) * (i + 2) / 2 + j
      array(index) += math.max(array(indexBelowStart), array(indexBelowStart + 1))
    }
    array(0)
  }

  import utils.Bench._

  //  pTimeIt(sum1(Euler018.lineNum, Euler018.triangle))
  //  pTimeIt(sum2(Euler018.lineNum, Euler018.triangle))
  pTimeIt(sum2(Euler067.lineNum, Euler067.triangle))


}
