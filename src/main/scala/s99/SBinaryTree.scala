package s99

object SBinaryTree {

  sealed abstract class Tree[+T]

  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString: String = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
  }

  case object End extends Tree[Nothing] {
    override def toString = "."
  }

  object Node {
    def apply[T](value: T): Node[T] = Node(value, End, End)
  }

  object Tree {
    private def log2(n: Int): Int = n match {
      case _ if n < 0 => throw new IllegalArgumentException
      case _ => 31 - Integer.numberOfLeadingZeros(n)
    }

    private def pow2(n: Int): Int = 1 << n

    // 55 TODO
    def cBalanced(n: Int, s: String): List[Node[String]] = {
//      val h = log2(n + 1)
//      val total = pow2(h) - 1
//      val remaining = n - total
      Nil
    }
  }

  // 54 omitted

}
