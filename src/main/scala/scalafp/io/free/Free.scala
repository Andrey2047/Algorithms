package scalafp.io.free

import scalafp.monad.Monad
import scalafp.paralelism.Par.Par

import scala.annotation.tailrec

sealed trait Free[F[_],A] {

  def map[B](f: A => B): Free[F, B] =
    flatMap(f.andThen(x => Return[F, B](x)))

  def flatMap[B](f: A => Free[F,B]): Free[F, B] =
    FlatMap(this, f)
}

case class Return[F[_],A](a: A) extends Free[F,A]

case class Suspend[F[_],A](s: F[A]) extends Free[F,A]
case class FlatMap[F[_],A,B](s: Free[F,A],
    f: A => Free[F,B]) extends Free[F,B]

object Free {
  type TailRec[A] = Free[Function0, A]
  type Async[A] = Free[Par, A]

  def freeMonad[F[_]]: Monad[({type f[a] = Free[F,a]})#f] =
    new Monad[({type f[a] = Free[F,a]})#f] {
      def unit[A](a: => A) = Return(a)
      def flatMap[A,B](fa: Free[F, A])(f: A => Free[F, B]) = fa.flatMap(f)
    }

  @tailrec
  def runTrampoline[A](a: Free[Function0,A]): A = {
    a match {
      case Return(x) => x
      case Suspend(fx) => fx()
      case FlatMap(s, f) => {
        s match {
          case Return(y) => runTrampoline(f(y))
          case Suspend(y) => runTrampoline(f(y()))
          case FlatMap(y, g) => runTrampoline(y flatMap(k => g(k).flatMap(f)))
        }
      }
    }
  }
}