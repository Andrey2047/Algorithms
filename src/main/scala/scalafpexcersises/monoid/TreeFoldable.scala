package scalafpexcersises.monoid

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
//
//object TreeFoldable extends Foldable[Tree] {
//
//  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = {
//    case Leaf(a) => f(a, z)
//    case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
//  }
//
//  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = {
//    as match {
//      case Leaf(a) => f(z, a)
//      case Branch(left, right) => foldLeft(right)(foldLeft(left)(z)(f))(f)
//    }
//  }
//
//  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = {
//    as match {
//      case Leaf(a) => f(a)
//      case Branch(left, right) => mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
//    }
//  }
//}
