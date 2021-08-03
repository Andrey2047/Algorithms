package scalafpexcersises.io.io

import scala.io.StdIn

case class Player(name: String, score: Int)

sealed trait IO[A] { self =>

  def flatMap[B](f: A => IO[B]): IO[B] =
    FlatMap(this, f)

  def map[B](f: A => B): IO[B] =
    flatMap(f andThen (Return(_)))

  def run(io: IO[A]): A = io match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) =>
        val value = f(a)
        run(value)
      case Suspend(r) => run(f(r()))
      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
    }
  }

}

case class Return[A](a: A) extends IO[A]
case class Suspend[A](resume: () => A) extends IO[A]
case class FlatMap[A,B](sub: IO[A], k: A => IO[B]) extends IO[B]

object IO {

  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0/9.0

  def unit[A](a: => A): IO[A] = new IO[A] { def run = a }

  def flatMap[A,B](fa: IO[A])(f: A => IO[B]) = fa flatMap f

  def printLine(s: String): IO[Unit] =
    Suspend(() => Return(println(s)))

  def apply[A](a: => A): IO[A] = unit(a)

  def ReadLine: IO[String] = IO { StdIn.readLine() }

  def PrintLine(msg: String): IO[Unit] = IO { println(msg) }

  def forever[A,B](a: IO[A]): IO[B] = {
    lazy val t: IO[B] = forever(a)
    a flatMap (_ => t)
  }

  def converter: IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  def c: IO[Unit] = {
    PrintLine("Enter a temperature in degrees Fahrenheit: ").flatMap(_ => {
      ReadLine.map(_.toDouble).flatMap(d => {
        PrintLine(fahrenheitToCelsius(d).toString)
      })
    })
  }


}