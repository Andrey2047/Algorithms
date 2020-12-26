package scalafp.monad

import scala.concurrent.{ ExecutionContext, Future }

object MonadLaws {

  trait Monad[T] {
    def flatMap[U](f: T => Monad[U]): Monad[U]
    def unit(x: T): Monad[T]
  }

  def squareFunction(x: Int): Option[Int] = Some(x * x)
  def incrementFunction(x: Int): Option[Int] = Some(x + 1)

  //left unit law
  //unit(x) flatMap f == f(x)

  def leftUnitLaw() {
    val x = 5
    val result = Some(x).flatMap(squareFunction) == squareFunction(x)
    println(result)
  }

  //right unit law
  //monad flatMap unit == monad

  def rightUnitLaw(): Unit = {
    val x = 5
    Some(x).flatMap(Option.apply) == Some(x)
  }

  //associative law
  //(monad flatMap f) flatMap g == monad flatMap(x => f(x) flatMap g)
  def assLaw(): Unit = {
    val x = 5
    Some(x).flatMap(squareFunction).flatMap(incrementFunction) == Some(x).flatMap(x => squareFunction(x).flatMap(incrementFunction))
  }

  def main(args: Array[String]): Unit = {

    implicit val ex = ExecutionContext.global

    val effect = Future {
      println("hello")
    }


    Seq(1,2,3).distinct

    val s = effect.flatMap(x =>
      Future(x)
    )

    s.value

    Thread.sleep(3000)
  }

}
