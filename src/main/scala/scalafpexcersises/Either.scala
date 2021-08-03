package scalafpexcersises

sealed trait Either[+E, +A] {

  def isRight: Boolean
  def isLeft: Boolean = !isRight

  def map[B](f: A => B): Either[E,B] = {
    this match {
      case Left(_) => this.asInstanceOf[Either[E,B]]
      case Right(value) => Right(f(value))
    }
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(_) => this.asInstanceOf[Either[EE, B]]
      case Right(value) => f(value)
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      av <- this
      bv <- b
    } yield f(av, bv)
  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(_) => b
      case _ => this
    }
  }

}
case class Left[+E](value: E) extends Either[E, Nothing] {
  override def isRight: Boolean = false
}
case class Right[+A](value: A) extends Either[Nothing, A] {
  override def isRight: Boolean = true
}