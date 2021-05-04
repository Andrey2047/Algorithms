package scalafpexcersises;

object FPinScalaS10 {

  trait Monoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
  }

  //ex 10.1

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1+a2
    override def zero: Int = 0
  }
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1*a2
    override def zero: Int = 1
  }
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = true
  }

  //ex 10.2

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
    override def zero: Option[A] = None
  }

  //ex 10.3
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a1.compose(a2)
    override def zero: A => A = identity
  }

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    forAll(for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x,y,z))(
      k => {
        m.op(m.op(k._1, k._2), k._3) == m.op(k._1, m.op(k._2, k._3))
      }
    ) && forAll(gen)(x => {
      m.op(x, m.zero) == x
    })
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  //ex 10.5
  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
  }

  //10.7
  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    def loop(input: IndexedSeq[A]):B = {
      if(input.isEmpty) {
        m.zero
      } else if(input.size == 1) {
        f(input.head)
      } else {
        val (left, right) = input.splitAt(input.size / 2)
        m.op(loop(left), loop(right))

      }
    }
    loop(v)
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
        case (Stub(c), Stub(d)) => Stub(c + d)
        case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
        case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
        case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
          Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
    }
    override def zero: WC = Stub("")
  }

  //10.11
  def count(in: String): Int = {
    def wc(c: Char): WC =
      if (c.isWhitespace)
        Part("", 0, "")
      else
        Stub(c.toString)

    def unstub(s: String): Int = s.length min 1

    foldMapV(in.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(s) => unstub(s)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }

  trait Foldable[F[_]] {
    def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B
    def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B
    def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B
    def concatenate[A](as: F[A])(m: Monoid[A]): A =
      foldLeft(as)(m.zero)(m.op)
  }

//  sealed trait Tree[+A]
//  case object Leaf[A](value: A) extends Tree[A]
//  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
//
//  object TreeFoldable extends Foldable[F[_]]

  def main(args: Array[String]): Unit = {
   // Prop.run(monoidLaws(booleanOr, Gen.boolean))

    println(foldMapV(Array(1.0, 2.0, 3.0, 4.0, 5.0), intAddition)(_.toInt))

    println(count("a1 a34 g544 kl88 z12 kl33"))
  }
}
