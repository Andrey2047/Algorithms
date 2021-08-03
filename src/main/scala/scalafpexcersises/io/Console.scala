package scalafpexcersises.io

import scalafpexcersises.io.free.Free
import scalafpexcersises.paralelism.Par
import scalafpexcersises.paralelism.Par.Par
import scalafpexcersises.io.free._
import scalafp.monad.Monad

sealed trait Console[A] {
  def toPar: Par[A]
  def toThunk: () => A
}
case object ReadLine extends Console[Option[String]] {
  def toPar = Par.lazyUnit(run)
  def toThunk = () => run
  def run: Option[String] =
    try Some[String](readLine().toString)
    catch { case e: Exception => None }
}
case class PrintLine(line: String) extends Console[Unit] {
  def toPar = Par.lazyUnit(println(line))
  def toThunk = () => println(line)
}

object Console {

  trait Translate[F[_], G[_]] { def apply[A](f: F[A]): G[A] }
  type ~>[F[_], G[_]] = Translate[F,G]

  type ConsoleIO[A] = Free[Console, A]
  def readLn: ConsoleIO[Option[String]] =
    Suspend(ReadLine)
  def printLn(line: String): ConsoleIO[Unit] =
    Suspend(PrintLine(line))

  val consoleToFunction0 =
    new (Console ~> Function0) { def apply[A](a: Console[A]) = a.toThunk }

  val consoleToPar =
    new (Console ~> Par) { def apply[A](a: Console[A]) = a.toPar }

//  def step[A](async: Free[Console, A]): Free[Console, A] = async match {
//    case FlatMap(FlatMap(x,f), g) => step(x flatMap (a => f(a) flatMap g))
//    case FlatMap(Return(x), f) => step(f(x))
//    case _ => async
//  }
//
//  def runFree[F[_],G[_],A](free: Free[F,A])(t: F ~> G)(
//      implicit G: Monad[G]): G[A] =
//    step(free) match {
//      case Return(a) => G.unit(a)
//      case Suspend(r) => t(r)
//      case FlatMap(Suspend(r),f) => G.flatMap(t(r))(a => runFree(f(a))(t))
//      case _ => sys.error("Impossible; `step` eliminates these cases")
//    }

  def main(args: Array[String]): Unit = {
    val f1: Free[Console, Option[String]] = for {
      _ <- printLn("I can only interact with the console.")
      ln <- readLn
    } yield ln
  }
}
