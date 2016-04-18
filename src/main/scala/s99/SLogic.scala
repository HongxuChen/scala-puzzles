package s99

object SLogic {

  // 46
  def and(a: Boolean, b: Boolean): Boolean = a match {
    case true => b
    case false => false
  }

  def or(a: Boolean, b: Boolean): Boolean = a match {
    case false => b
    case true => true
  }

  def nand(a: Boolean, b: Boolean) = !and(a, b)

  def nor(a: Boolean, b: Boolean) = !or(a, b)

  def xor(a: Boolean, b: Boolean) = if (a == b) false else true

  def impl(a: Boolean, b: Boolean) = a match {
    case false => true
    case true => b
  }

  def equ(a: Boolean, b: Boolean) = !xor(a, b)

  def table(f: (Boolean, Boolean) => Boolean) = {
    println("A\t\tB\t\tresult")
    for (a <- Array(true, false); b <- Array(true, false)) {
      println(f"$a%8b$b%8b${f(a, b)}")
    }
  }

  // 47 TODO

  // 48 TODO omitted

  // 49
  val grayList = List("0", "1")

  def gray(n: Int): List[String] = n match {
    case _ if n < 1 => throw new IllegalArgumentException
    case 1 => grayList
    case _ => {
      val l = gray(n - 1)
      l.map("0" + _) ::: l.reverse.map("1" + _)
    }
  }

  // 50 TODO
  def huffman(l: List[(String, Int)]): List[(String, String)] = {
    abstract class CodeTree
    val sortedList = l.sortWith((a, b) => a._2 < b._2)
    Nil
  }


}
