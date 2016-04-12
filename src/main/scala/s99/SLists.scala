package s99

import java.util.NoSuchElementException

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object SLists {

  // 01
  @tailrec
  def last[A](l: List[A]): A = l match {
    case Nil => throw new NoSuchElementException
    case head :: Nil => head
    case head :: tail => last(tail)
  }

  // 02
  @tailrec
  def penultimate[A](l: List[A]): A = l match {
    case Nil => throw new NoSuchElementException
    case head :: Nil => throw new NoSuchElementException
    case res :: last :: Nil => res
    case head :: tail => penultimate(tail)
  }

  // 03
  def nth[A](n: Int, l: List[A]): A = {
    def inner(k: Int, l: List[A]): A = k match {
      case `n` => l.head
      case _ => l match {
        case Nil => throw new IndexOutOfBoundsException
        case head :: tail => inner(k + 1, tail)
      }
    }
    n match {
      case i if i < 0 => throw new IndexOutOfBoundsException
      case _ => inner(0, l)
    }
  }

  // 04
  def length[A](l: List[A]): Int = {
    @tailrec
    def inner(l: List[A], length: Int): Int = l match {
      case Nil => length
      case head :: tail => inner(tail, length + 1)
    }
    inner(l, 0)
  }

  // 05
  def reverse[A](l: List[A]): List[A] = {
    @tailrec
    def inner(in: List[A], out: List[A]): List[A] = in match {
      case Nil => out
      case head :: tail => inner(tail, head :: out)
    }
    inner(l, Nil)
  }

  // 06
  def isPalindrome[A](l: List[A]): Boolean = reverse(l) == l

  // 07
  def flatten(l: List[Any]): List[Any] = l flatMap {
    case list: List[_] => flatten(list)
    case e => List(e)
  }

  // 08
  // FIXME complexity
  def compress[A](l: List[A]): List[A] = {
    if (l.length < 2) l
    else {
      var previous = l.head
      val bl = ListBuffer(previous)
      for (current <- l.tail) {
        if (previous != current) {
          bl += current
        }
        previous = current
      }
      bl.toList
    }
  }

  private def eleCounter[A](l: List[A]): List[(A, Int)] = {
    type PackTy = (A, Int)
    var previous = l.head
    var num = 1
    val bl = ListBuffer.empty[PackTy]
    for (current <- l.tail) {
      if (previous != current) {
        bl += previous -> num
        num = 1
      } else {
        num += 1
      }
      previous = current
    }
    bl += previous -> num
    bl.toList
  }

  // 09
  def pack[A](l: List[A]): List[List[A]] = {
    l.length match {
      case 0 => Nil
      case _ => eleCounter(l).map(e => List.fill(e._2)(e._1))
    }
  }

  // 10
  def encode[A](l: List[A]): List[(Int, A)] = pack(l).map(e => (e.length, e.head))

  // 11
  def encodeModified[A](l: List[A]): List[Any] = pack(l) map {
    e => e.length match {
      case 1 => e.head
      case _ => e.length -> e.head
    }
  }

  // 12
  def decode[A](l: List[(Int, A)]): List[A] = l flatMap {
    case (count, e) => List.fill(count)(e)
  }

  // 13
  def encodeDirect[A](l: List[A]): List[(Int, A)] = l match {
    case Nil => Nil
    case _ => eleCounter(l).map(e => (e._2, e._1))
  }

  // 14
  def duplicate[A](l: List[A]): List[A] = duplicateN(2, l)

  // 15
  def duplicateN[A](n: Int, l: List[A]): List[A] = l flatMap {
    case e => List.fill(n)(e)
  }

  //////////////////////////////////////////////////////////////////////
  // 16
  def drop[A](n: Int, l: List[A]): List[A] = for ((e, i) <- l.zip(l.indices.map(_ % n)); if i + 1 != n) yield e

  // 17
  def split[A](n: Int, l: List[A]): (List[A], List[A]) = {
    val left, right = ListBuffer.empty[A]
    for ((e, i) <- l.zipWithIndex) {
      if (i < n) left += e
      else right += e
    }
    left.toList -> right.toList
  }

  // 18
  def slice[A](start: Int, end: Int, l: List[A]): List[A] = for ((e, i) <- l.zipWithIndex; if i >= start && i < end) yield e


  // 19
  def rotate[A](n: Int, l: List[A]): List[A] = {
    val pivot = if (n < 0) length(l) + n else n
    val (left, right) = split(pivot, l)
    right ++ left
  }

  // 20
  def removeAt[A](n: Int, l: List[A]): (List[A], A) = {
    val (left, right) = split(n, l)
    (left ++ right.tail, right.head)
  }

}
