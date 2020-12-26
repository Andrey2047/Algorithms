package trampoling

import scala.annotation.tailrec
import scala.util.control.TailCalls.TailRec


//Defines if the number odd or event in specific way

object OddEventExample {

  def even(i: Int): Boolean = i match {
    case 0 => true
    case _ => odd(i - 1)
  }

  def odd(i: Int): Boolean = i match {
    case 0 => false
    case _ => even(i - 1)
  }

  sealed trait EvenOdd
  case class Done(result: Boolean) extends EvenOdd
  case class Even(value: Int) extends EvenOdd
  case class Odd(value: Int) extends EvenOdd

  def evenT(i: Int): EvenOdd = i match {
    case 0 => Done(true)
    case _ => Odd(i - 1)
  }

  def oddT(i: Int): EvenOdd = i match {
    case 0 => Done(false)
    case _ => Even(i - 1)
  }

  @tailrec
  def run(evenOdd: EvenOdd): Boolean = evenOdd match {
    case Done(result) => result
    case Even(value) => run(evenT(value))
    case Odd(value) => run(oddT(value))
  }

  import scala.util.control.TailCalls.{ TailRec, done, tailcall }

  def evenS(i: Int): TailRec[Boolean] = i match {
    case 0     => done(true)
    case _ => tailcall(oddS(i - 1))
  }

  def oddS(i: Int): TailRec[Boolean] = i match {
    case 0     => done(false)
    case _ => tailcall(evenS(i - 1))
  }

  def main(args: Array[String]): Unit = {
    //we will run stack overflow
    //println(even(500001))

    //Calculate using trampoling technic
    println(run(evenT(500000001)))

  }

}
