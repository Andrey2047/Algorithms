package scalafp.monoid

import scala.Option

object OptionFoldable extends Foldable[Option]{

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
    case None => z
    case Some(a) => f(a, z)
  }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
    case None => z
    case Some(a) => f(z, a)
  }
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = {
     as match {
       case Some(v) => f(v)
       case None => mb.zero
     }
  }
}
