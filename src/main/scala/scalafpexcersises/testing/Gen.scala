package scalafp.testing

import scalafp.state.State

case class SGen[A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)
}

case class Gen[A](sample: State[RNG, A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(this.sample.flatMap(f(_).sample))
  }

  def map[B](f: A => B): Gen[B] = {
    Gen(this.sample.map(f))
  }

  def map2[B, C](g2: Gen[B])(f: (A, B) => C): Gen[C] = {
    Gen(this.sample.map2(g2.sample)(f))
  }

  def listOfN2[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))


  //Ex 8.10
  def unsized: SGen[A] = SGen(_ => this)

  def listOfSGen(g: Gen[A]): SGen[List[A]] = {
    SGen(n => g.listOfN2(n, g))
  }

  def **[B](g: Gen[B]): Gen[(A,B)] =
    (this map2 g)((_,_))

}

object Gen {

  //EX 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))
  }

  //Ex 8.5
  def unit[A](a: => A): Gen[A] = {
    Gen(State(RNG.unit(a)))
  }

  def boolean: Gen[Boolean] = {
    Gen(State(RNG.int).map(_ % 2 == 0))
  }

  def string: Gen[String] = ???

  def int: Gen[Int] = {
    Gen(State(RNG.int))
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    Gen(State.sequence(List.fill(n)(g.sample)))
  }

  //Ex 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    boolean.flatMap(if (_) g1 else g2)
  }

  //Ex 8.8
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    Gen(State(RNG.double).flatMap(d => {
      if (d < (g1._2 / (g1._2 + g2._2))) g1._1.sample
      else g2._1.sample
    }))
  }

  /**
    * Exercise 12
    * We can now implement a listOf combinator that does not accept an explicit
    * size. It can return an SGen instead of a Gen. The implementation can
    * generate lists of the requested size.
    */
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(Gen.listOfN(_, g))

  /**
    * Exercise 14
    * Define listOf1, for generating nonempty lists, then update your
    * specification of max to use this generator.
    */
  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => Gen.listOfN(n max 1, g))
}

