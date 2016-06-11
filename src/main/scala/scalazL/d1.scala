package scalazL

import scalaz.Scalaz._
import scalaz._

object d1 extends App {

  Int.MaxValue.succx

  implicitly[Enum[Char]].min
  implicitly[Enum[Char]].max

  trait CanTruthy[A] {
    self =>
    def truthys(a: A): Boolean
  }

  object CanTruthy {
    def apply[A](implicit ev: CanTruthy[A]): CanTruthy[A] = ev

    def truthys[A](f: A => Boolean): CanTruthy[A] = new CanTruthy[A] {
      override def truthys(a: A): Boolean = f(a)
    }
  }

  trait CanTruthyOps[A] {
    def self: A

    implicit def F: CanTruthy[A]

    final def truthy: Boolean = F.truthys(self)
  }

  object ToCanTruthyOps {
    implicit def toCanTruthyOps[A](v: A)(implicit ev: CanTruthy[A]) =
      new CanTruthyOps[A] {
        override def self: A = v

        override implicit def F: CanTruthy[A] = ev
      }
  }

  implicit val intCanTruthy: CanTruthy[Int] = CanTruthy.truthys({
    case 0 => false
    case _ => true
  })

  implicit def listCanTruthy[A]: CanTruthy[List[A]] = CanTruthy.truthys({
    case Nil => false
    case _ => true
  })

  implicit val nilCanTruthy: CanTruthy[collection.immutable.Nil.type] = CanTruthy.truthys(_ => false)

  implicit val booleanCanTruthy: CanTruthy[Boolean] = CanTruthy.truthys(identity)

  import ToCanTruthyOps._

  10.truthy
  List("foo").truthy
  Nil.truthy

  false.truthy

  def truthyIf[A: CanTruthy, B, C](cond: A)(ifyes: => B)(ifno: => C) =
    if (cond.truthy) ifyes else ifno

  truthyIf(Nil) {
    "YEAH!"
  } {
    "NO!"
  }
  truthyIf(2 :: 3 :: 4 :: Nil) {
    "YEAH!"
  } {
    "NO!"
  }

}