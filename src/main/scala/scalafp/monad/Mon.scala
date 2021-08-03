package scalafp.monad

import scalafpexcersises.testing.Gen
import scalafpexcersises.stream.Stream

trait Mon[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))
  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]
  def map2[A,B,C](
      fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a,b)))

  def unit[A](fa: A): F[A]
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]
  def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

//  def sequence[A](lma: List[F[A]]): F[List[A]] = {
//    traverse(lma)(identity)
//  }

//  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] = {
//    val cc = unit(List.empty[A])
//    la.foldLeft(cc)((b,a) => {
//      map2(b, f(a))(_ ++ _)
//    })
//  }

//  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
//    sequence(List.fill(n)(ma))

  def product[A,B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = {
    a => {
      flatMap(f(a))(g)
    }
  }

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(identity)


//  compose(compose(f, g), h) == compose(f, compose(g, h))
//  compose(f, unit) == f
//  compose(unit, f) == f
//
//  flatMap(x)(unit) == x
//  flatMap(unit(y))(f) == f(y)
}

object Monad {

  case class Id[A](value: A) {
    def flatMap[B](f: A => Id[B]): Id[B] = {
      f(value)
    }
  }

  val idMonad = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)
    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma.flatMap(f)
  }

  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val listMonad = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)
    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
  }

  val streamMonad = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream.empty
    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma.flatMap(f)
  }
}