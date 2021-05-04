
import scalafp.option.Some
import scalafp.{ option, _ }

def mean(xs: Seq[Double]): option.Option[Double] = {
  if (xs.isEmpty) {
    option.None
  } else {
    option.Some(xs.sum / xs.length.toDouble)
  }
}

//4.2
def variance(xs: Seq[Double]): option.Option[Double] = {
  val f: (Double, Double) => Double = (m, x) => math.pow(x - m, 2)
  mean(xs).flatMap(x =>
    mean(xs.map(f(x, _)))
  )
}

//4.3
def map2[A,B,C](a: option.Option[A], b: option.Option[B])(f: (A, B) => C): option.Option[C] = {
  (a, b) match {
    case (Some(av), Some(bv)) => Some(f(av, bv))
    case _ => option.None
  }
}

//4.4
def sequence[A, B](a: List[option.Option[A]]): option.Option[List[B]] = {
  a.foldRight(option.Option(List.empty[A]))((a:option.Option[A], b:option.Option[List[B]]) => {
    map2(a, b)(_ :: _)
  })
}

sequence(List(Some(2), Some(4), Some(3)))

//4.5
def traverse[A, B](a: List[A])(f: A => option.Option[B]): option.Option[List[B]] = {
  a.foldRight(option.Option(List.empty[A]))((a:A, b:option.Option[List[B]]) => {
    (f(a), b) match {
      case (Some(x), Some(y)) => Some(x :: y)
      case _ => option.None
    }
  })
}

//4.7
def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
  es.foldRight(Either[E, List[A]](List.empty[A]))((a: Either[E, A], b: Either[E, List[A]])=>{
    b.map2(a)(_ :+ _)
  })
}

def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {

  as.foldRight(Either[E, List[B]](List.empty[A]))((a:A, b:Either[E, List[B]]) => {
    (f(a), b) match {
      case (Right(x), Right(y)) => Right(x :: y)
      case (l@Left(_), _) => l
      case (_, l@Left(_)) => l
    }
  })
}
