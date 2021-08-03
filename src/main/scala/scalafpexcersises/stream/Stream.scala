package scalafpexcersises.stream


sealed trait Stream[+A] {

  //Ex.1
  def toList: List[A] = {
    def loop(stream: Stream[A], acc: List[A]): List[A] = {
      stream match {
        case Empty => acc
        case Cons(h, t) => loop(t(), h() :: acc)
      }
    }

    loop(this, Nil)
  }

  //ex.5.2
  def take(n: Int): Stream[A] = {
    //    this match {
    //      case Cons(head, tail) if n > 0 => Stream.cons(head(), tail().take(n-1))
    //      case _ => Stream.empty[A]
    //    }
    unfold((this, n)) {
      case (Empty, _) => None
      case (_, 0) => None
      case (Cons(h, t), i) => Some((h(), (t(), i - 1)))
    }
  }


  //ex 5.3
  def drop(n: Int): Stream[A] = {
    this match {
      case Cons(_, tail) if n > 0 => tail().drop(n - 1)
      case _ => this
    }
  }

  //ex 5.4
  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(head, tail) if p(head()) => Stream.cons[A](head(), tail().takeWhile(p))
      case _ => Stream.empty[A]
    }
  }

  def dropWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) if p(h()) => t().dropWhile(p)
      case _ => this
    }
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  //5.4
  def forAll(p: A => Boolean): Boolean = {
    this match {
      case Cons(h, t) => p(h()) && t().forAll(p)
      case _ => true
    }
  }

  //ex.5.5
  def takeWhileFR(p: A => Boolean): Stream[A] = {

    def step(x: A, xs: => Stream[A]): Stream[A] =
      if (p(x)) {
        print(" StreamConst(x .. ")
        Stream.cons(x, xs)
      } else {
        print(" StreamE ")
        Stream.empty[A]
      }

    this.foldRight(Stream.empty[A])(step)

  }

  def printSt: Unit = {
    this match {
      case Cons(h, t) => print(h() + " "); t().printSt
      case _ => println()
    }
  }

  //5.7
  def append[A1 >: A](b: Stream[A1]): Stream[A1] = {
    this.foldRight(b)((a, b) => Stream.cons(a, b))
  }

  def map[B](f: A => B): Stream[B] = {
    //this.foldRight(Stream.empty[B])((a, b) => Stream.cons[B](f(a), b))
    unfold(this) {
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    }
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    this.foldRight(Stream.empty[B])((a, b) => f(a).append(b))
  }

  def filter(f: A => Boolean): Stream[A] = {
    this.foldRight(Stream.empty[A])((a, b) => {
      if (f(a)) {
        Stream.cons(a, b)
      } else {
        b
      }
    })
  }

  def headOption: scala.Option[A] = {
    this.foldRight(scala.Option.empty[A])((a, _) => scala.Some(a))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => Stream.empty[A]
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    }
  }

  //5.15
  def tails: Stream[Stream[A]] = {
    unfold(this) {
      case Empty => None
      case or@Cons(_, t) => Some((or, t()))
    }
  }

  def zipWith[B,C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None: Option[B]), (t1(), Empty))
      case (Empty, Cons(h2, t2)) => Some((None: Option[A], Some(h2())), (Empty, t2()))
      case _ => None
    }

  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2)((_,_))

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => Stream.empty[A]
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    }
  }

  def from(n: Int): Stream[Int] = {
    cons(n, from(n+1))
  }

}
