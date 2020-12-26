
sealed trait Validation[+E, +A] {
  def +++[EE >: E, AA >: A](x: => Validation[EE, AA])(implicit
      ap1: Appendable[AA],
      ap2: Appendable[EE]): Validation[EE, AA] = {
    (this, x) match {
      case (Failure(y), Failure(z)) => Failure(ap2.append(y, z))
      case (f1@Failure(_), _) => f1
      case (_, f2@Failure(_)) => f2
      case (Success(y), Success(z)) => Success(ap1.append(y, z))
    }
  }

  def isSuccess: Boolean
  def isFailure: Boolean = !isSuccess
}

final case class Success[A](value: A) extends Validation[Nothing, A] {
  def isSuccess: Boolean = true
}

final case class Failure[E](value: E) extends Validation[E, Nothing] {
  def isSuccess: Boolean = false
}

trait Appendable[A] {
  def append(a: A, b: A): A
}

object Appendable {
  implicit def intAppend: Appendable[Int] = new Appendable[Int] {
    def append(a: Int, b: Int): Int = a + b
  }

  implicit def stringAppend: Appendable[String] = new Appendable[String] {
    def append(a: String, b: String): String = a + b
  }

  implicit def seqAppend[A]: Appendable[List[A]] = new Appendable[List[A]] {
    def append(a: List[A], b: List[A]): List[A] = a ++ b
  }
}

