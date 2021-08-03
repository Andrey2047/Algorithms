package scalafpexcersises.state

case class State[S,+A](run: S => (A, S)) {

  def map[B](f: A => B): State[S,B] =
    flatMap(a => State.unit(f(a)))

  def map2[B,C](sb: State[S,B])(f: (A, B) => C): State[S,C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S,B]): State[S,B] =
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
}

object State {

  def unit[S,A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] =
    fs.foldRight(unit[S,List[A]](Nil)) { (f, acc) =>
      f.map2(acc)(_ :: _)
    }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

}
object M {
  //Ex. 11
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int) {
    def noCandies: Boolean = candies == 0
  }

  type State[S, +A] = S => (A, S)
  type MchState = State[Machine, (Int, Int)]

  private val doNothing: MchState = mch => ((mch.coins, mch.candies), mch)

  private val addCoin: MchState = {
    case mch @ Machine(true, ca, co) if !mch.noCandies =>
      ((co + 1, ca), Machine(false, ca, co + 1))
    case mch => doNothing(mch)
  }

  private val turnKnob: MchState = {
    case mch @ Machine(false, ca, co) if !mch.noCandies =>
      ((co, ca - 1), Machine(true, ca - 1, co) )

    case mch => doNothing(mch)
  }

  private def runStep(inp: Input): MchState = inp match {
    case Coin => addCoin
    case Turn => turnKnob
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    inputs match {
      case List() => doNothing
      case i :: is =>
        mch => {
          val (_, m) = runStep(i)(mch)
          simulateMachine(is)(m)
        }
    }
  }
}

