package scalafpexcersises;

object FPinScalaS5 {

  def main(args: Array[String]): Unit = {

    stream.Stream(1,2).map(_ + 1).filter(_ > 2)

    //ex 5.4
    println(stream.Stream[Int](1,2,3,4).forAll(x => x > 10))

    val exSt = stream.Stream[Int](1, 2, 3, 4, 5)
    exSt.takeWhileFR(_ < 4).printSt

    exSt.map(_+1).printSt

    exSt.filter(_ % 2 != 0).printSt
    exSt.flatMap(x => stream.Stream(x,x)).printSt

    lazy val ones: scala.Stream[Int] = scala.Stream.cons[Int](1, ones)

    println(fibsUnf.take(10).toList.reverse)
    println(constantUnf(3).take(10).toList)
    println(fromUnf(5).take(10).toList.reverse)
    println(stream.Stream.apply(1,2,3,4).tails.map(_.toList).toList)
  }

  //Ex. 5.8
  def constant[A](a: A): stream.Stream[A] = {
    lazy val st: stream.Stream[A] = cons(a, st)
    st
  }

  //Ex 5.9
  def from(n: Int): stream.Stream[Int] = {
    cons(n, from(n+1))
  }

  private def nextFib(n1: Int, n2: Int): stream.Stream[Int] = {
    stream.Stream.cons(n1, nextFib(n2, n1+n2))
  }

  //5.10
  def fibs: stream.Stream[Int] = nextFib(0, 1)

  //5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): stream.Stream[A] = {
    f(z) match {
      case None => stream.Stream.empty[A]
      case Some((a, s)) => stream.Stream.cons(a, unfold(s)(f))
    }
  }

  //5.12
  def constantUnf[A](a: A): stream.Stream[A] = {
    unfold(a)(a => Some(a,a))
  }

  def fromUnf(n: Int): stream.Stream[Int] = {
    unfold(n)(k => Some((k, k+1)))
  }

  def fibsUnf: stream.Stream[Int] = unfold((0,1))(a => {
    val next = a._1 + a._2
    Some(a._1 + a._2, (a._2, next))
  })

}
