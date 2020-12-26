package scalafp

import scalafp.state.M
import scalafp.state.M.{ Coin, Turn }
import scalafp.testing.{ RNG, SimpleRNG }

object FPInScalaS6 {

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = rng1.nextDouble
    ((i, d), rng2)
  }

  private def toDouble(n: Int): Double = n match {
    case Int.MaxValue | Int.MinValue => 0.0
    case i                           => math.abs(i) / Int.MaxValue.toDouble
  }

  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(42)

    val r = RNG.map2(_.nextInt, _.nextDouble)(_ + _)

    println(r(SimpleRNG(42)))

    println(M.simulateMachine(List(Coin, Turn))(M.Machine(true, 1, 0))._1)


  }

  //Ex.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n1, rng2) = rng.nextInt
    (abs2(n1), rng2)
  }

  private def abs2(n: Int): Int = n match {
    case Int.MinValue => 0
    case _ if n >= 0  => n
    case _ if n < 0   => math.abs(n)
  }

}
