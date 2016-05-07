object d0 extends App {

  trait Monoid[A] {
    def mappend(a1: A, a2: A): A

    def mzero: A
  }

  class IntMonoid extends Monoid[Int] {
    override def mappend(a1: Int, a2: Int): Int = a1 + a2

    override def mzero: Int = 0
  }

  object StringMonoid extends Monoid[String] {
    override def mappend(a1: String, a2: String): String = a1 + a2

    override def mzero: String = ""
  }

  object MyList {
    def foldl[A, B](xs: List[A], b: B, op: (B, A) => B) = xs.foldLeft(b)(op)
  }

  trait FoldLeft[F[_]] {
    def foldLeft[A, B](xs: F[A], b: B, op: (B, A) => B): B
  }

  object FoldLeft {
    implicit val foldLeftList: FoldLeft[List] = new FoldLeft[List] {
      override def foldLeft[A, B](xs: List[A], b: B, op: (B, A) => B): B = (b /: xs) (op)
    }
  }

  def sum[A: Monoid](xs: List[A]) = {
    val m = implicitly[Monoid[A]]
    MyList.foldl(xs, m.mzero, m.mappend)
    //  (m.mzero /: xs) (m.mappend)
  }

  def sum2[M[_] : FoldLeft, A: Monoid](xs: M[A]): A = {
    val m = implicitly[Monoid[A]]
    val f = implicitly[FoldLeft[M]]
    f.foldLeft(xs, m.mzero, m.mappend)
  }


  implicit val multiMonoid = new Monoid[Int] {
    override def mappend(a1: Int, a2: Int): Int = a1 * a2

    override def mzero: Int = 1
  }

  implicit val stringMonoid = StringMonoid

  sum(List(1, 2, 3, 4))(new IntMonoid)
  sum2(List(1, 2, 3, 4))
  sum2(List("Good", "String"))


  trait MonoidOp[A] {
    val F: Monoid[A]
    val value: A

    def |+|(a2: A) = F.mappend(value, a2)
  }

  implicit def toMonoidOp[A: Monoid](a: A): MonoidOp[A] = new MonoidOp[A] {
    override val value: A = a
    override val F: Monoid[A] = implicitly[Monoid[A]]
  }

  3 |+| 4

  "3" |+| "4"

  import scalaz.Scalaz._

  1.some | 3
  (1 > 10) ? 1 | 2

}