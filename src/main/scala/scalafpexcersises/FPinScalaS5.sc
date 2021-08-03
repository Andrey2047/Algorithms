def maybeTwice(b: Boolean, i: => Int) = {
  lazy val j = i
  if (b) j + j else 0
}

val x = maybeTwice(true, { println("hi"); 1+41 })

val s = Stream(Seq(1,2,4,5))

s.toList


//ex 5.5
//def forAll[A](xs: Stream[A])(p: A => Boolean): Boolean = {
//  xs match {
//    case Cons(h, t) => p(h) && forAll(t())(p)
//    case _ => false
//  }
//}

//ex 5.4

import scalafpexcersises._

