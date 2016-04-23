package euler.a1


import scala.io.Source

object Euler018 {
  val triangleStr = "75\n95 64\n17 47 82\n18 35 87 10\n20 04 82 47 65\n19 01 23 75 03 34\n88 02 77 73 07 63 67\n99 65 04 28 06 16 70 92\n41 41 26 56 83 40 80 70 33\n41 48 72 33 47 32 37 16 94 29\n53 71 44 65 25 43 91 52 97 51 14\n70 11 33 28 77 73 17 78 39 68 17 57\n91 71 52 38 17 14 91 43 58 50 27 29 48\n63 66 04 68 89 53 67 30 73 16 69 87 40 31\n04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"
  val triangle = triangleStr.split("\\s+").map(_.toInt)
}

object Euler067 {

  import java.io.File

  private val file = {
    val src = System.getProperty("user.dir") + s"${File.separator}src${File.separator}main${File.separator}scala"
    val dir = getClass.getName.replaceAll("\\.\\w+\\$$", "").replace('.', File.separatorChar)
    src + File.separator + dir + File.separator + "p067_triangle.txt"
  }

  private var counter = 0
  val triangle = for {
    line <- Source.fromFile(file).getLines().toArray
    num <- line.split("\\s+").map(_.toInt)
  } yield {
    counter += 1
    num
  }
  val linNum = counter
}

object Euler_TriangleSum extends App {


  def next(cur: Int, height: Int) = (cur + height, cur + height + 1)

  def sum(height: Int, array: Array[Int]): Int = {
    def inner(i: Int, j: Int): Int = {
      val index = i * (i + 1) / 2 + j
      //      println(s"$i, $j, $index, ${triangle(index)}")
      i + 1 match {
        case `height` => array(index)
        case _ => {
          //          println(s"($i, $j), ${inner(i + 1, j)}, ${inner(i + 1, j + 1)}")
          val max = math.max(inner(i + 1, j), inner(i + 1, j + 1))
          array(index) + max
        }
      }
    }
    inner(0, 0)
  }

  def getCurrentDirectory = new java.io.File(".").getCanonicalPath


  //  println(sum(15, Euler018.triangle))
    println(sum(Euler067.linNum, Euler067.triangle))


}
