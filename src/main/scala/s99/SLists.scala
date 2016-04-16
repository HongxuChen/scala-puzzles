package s99

import scala.annotation.tailrec
import scala.collection.mutable

object SLists {


  // 01
  @tailrec
  def last[A](xs: List[A]): A = xs match {
    case Nil => throw new IndexOutOfBoundsException
    case List(x) => x
    case y :: ys => last(ys)
  }

  //////////////////////////////////////////////////////
  def init[A](xs: List[A]): List[A] = xs match {
    case Nil => throw new IndexOutOfBoundsException
    case List(x) => Nil
    case y :: ys => y :: init(ys)
  }

  // TODO tailrec???
  def concat[A](xs: List[A], ys: List[A]): List[A] = xs match {
    case Nil => ys
    case z :: zs => z :: concat(zs, ys)
  }

  def msort[A](xs: List[A])(implicit ord: Ordering[A]): List[A] = {
    def merge(xs: List[A], ys: List[A]): List[A] = (xs, ys) match {
      case (Nil, _) => ys
      case (_, Nil) => xs
      case (x :: xs1, y :: ys1) => if (ord.lt(x, y)) x :: merge(xs1, ys) else y :: merge(xs, ys1)
    }
    xs.length / 2 match {
      case 0 => xs
      case n => {
        val (fst, snd) = xs splitAt n
        merge(msort(fst), msort(snd))
      }
    }
  }

  def mapFun[T, U](xs: List[T], f: T => U): List[U] = (xs foldRight List.empty[U]) ((a: T, b: List[U]) => f(a) :: b)

  def lengthFun[T](xs: List[T]): Int = (xs foldRight 0) ((_, len) => 1 + len)


  //////////////////////////////////////////////////////

  // 02
  @tailrec
  def penultimate[A](l: List[A]): A = l match {
    case Nil => throw new IndexOutOfBoundsException
    case head :: Nil => throw new IndexOutOfBoundsException
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
  def compress[A](l: List[A]): List[A] = {
    if (l.length < 2) l
    else {
      var previous = l.head
      val bl = mutable.ListBuffer(previous)
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
    val bl = mutable.ListBuffer.empty[PackTy]
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

  def pack[A](xs: List[A]): List[List[A]] = xs match {
    case Nil => Nil
    case x :: xs1 => {
      val (first, rest) = xs span (y => y == x)
      first :: pack(rest)
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

  // 16
  def drop[A](n: Int, l: List[A]): List[A] = for ((e, i) <- l.zip(l.indices.map(_ % n)); if i + 1 != n) yield e

  // 17
  def split[A](n: Int, l: List[A]): (List[A], List[A]) = {
    val left, right = mutable.ListBuffer.empty[A]
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

  // 21
  def insertAt[A](e: A, n: Int, l: List[A]): List[A] = {
    val (left, right) = split(n, l)
    (left :+ e) ++ right
  }

  // 22
  def range(start: Int, end: Int): List[Int] = (start to end).toList

  // 23
  def randomSelect[A](n: Int, l: List[A]): List[A] = {
    val r = util.Random
    val fullLength = l.length
    var counter = 0
    var previous = l
    val lb = mutable.ListBuffer.empty[A]
    while (counter < n) {
      val index = r.nextInt(fullLength - counter)
      val res = removeAt(index, previous)
      previous = res._1
      lb += res._2
      counter += 1
    }
    lb.toList
  }

  // 24
  def lotto(n: Int, max: Int): List[Int] = randomSelect(n, (1 to max).toList)

  // 25
  def randomPermute[A](l: List[A]): List[A] = randomSelect(l.length, l)

  // 26
  def combinations[A](n: Int, l: List[A]): List[List[A]] = {
    val length = l.length
    if (n <= 0 || n > length) Nil
    else if (n == 0 || n == length) List(l)
    else {
      combinations(n - 1, l.tail).map(l.head :: _) ::: combinations(n, l.tail)
    }
  }

  // 27 TODO
  def group[A](s: List[A], l: List[A]): List[List[List[A]]] = {
    Nil
  }

  // 28
  def lsort[A](l: List[List[A]]): List[List[A]] = l.sortBy(_.length)

  def lsortFreq[A](l: List[List[A]]): List[List[A]] = {
    val compound = l.map(i => i -> i.length)
    val m = mutable.Map.empty[Int, Int]
    for ((_, len) <- compound) {
      val freq = m.getOrElse(len, 0)
      m += len -> (freq + 1)
    }
    compound.sortWith((a, b) => m(a._2) < m(b._2)).map(_._1)
  }

}
