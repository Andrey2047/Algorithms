package scalafpexcersises.testing

trait RNG {

  def nextInt: (Int, RNG)

  def nextDouble: (Double, RNG)

}


object RNG {

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  val double: Rand[Double] = _.nextDouble

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(i => unit(f(i)))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (v, s) = f(rng)
      g(v)(s)
    }
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  //ex 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List.empty[A]))((a: Rand[A], b: Rand[List[A]]) => {
      map2(a,b)(_ :: _)
    })
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = rng1.nextDouble
    ((i, d), rng)
  }


  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (i, rng1) = rng.nextDouble
    val (d, rng2) = rng1.nextInt
    ((i, d), rng)
  }


  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng1) = rng.nextDouble
    val (d2, rng2) = rng1.nextDouble
    val (d3, rng3) = rng2.nextDouble

    ((d1,d2,d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    (1 to count).foldLeft(List.empty[Int], rng)((acc:(List[Int], RNG), _) => {
      val (i, s) = acc._2.nextInt
      (acc._1 :+ i, s)
    })
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n1, rng2) = rng.nextInt
    (abs2(n1), rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    map(nonNegativeInt) { i => i % n }

  private def abs2(n: Int): Int = n match {
    case Int.MinValue => 0
    case _ if n >= 0  => n
    case _ if n < 0   => math.abs(n)
  }

  def simple(seed: Long): RNG = {
    SimpleRNG(seed)
  }
}

case class SimpleRNG(seed: Long) extends RNG {

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  def nextDouble: (Double, RNG) = {
    def toDouble(n: Int): Double = {
      math.abs(n) / Int.MaxValue.toDouble
    }
    val (n1, rng2) = nextInt
    (toDouble(n1), rng2)
  }
}

