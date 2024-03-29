package scalafpexcersises.io.par

import java.util.concurrent.{ ExecutorService, Executors }

import scalafpexcersises.paralelism.Par
import scalafpexcersises.paralelism.Par.Par

sealed trait Async[A] {
  def flatMap[B](f: A => Async[B]): Async[B] =
    FlatMap(this, f)
  def map[B](f: A => B): Async[B] =
    flatMap(f andThen (Return(_)))
}

case class Return[A](a: A) extends Async[A]
case class Suspend[A](resume: Par[A]) extends Async[A]
case class FlatMap[A,B](sub: Async[A],
    k: A => Async[B]) extends Async[B]


object Async {

  @annotation.tailrec
  def step[A](async: Async[A]): Async[A] = async match {
    case FlatMap(FlatMap(x,f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => async
  }

  def run[A](async: Async[A]): Par[A] = step(async) match {
    case Return(a) => Par.unit(a)
    case Suspend(r) => Par.flatMap(r)(a =>
      Par.unit(a)
    )
    case FlatMap(x, f) => x match {
      case Suspend(r) => Par.flatMap(r)(a => run(f(a)))
      case _ => sys.error("Impossible; `step` eliminates these cases")
    }
  }

}