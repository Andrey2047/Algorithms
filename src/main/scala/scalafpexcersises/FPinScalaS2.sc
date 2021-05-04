import scala.annotation.tailrec

val x=1


def factorial(n: Int): Int = {

  @tailrec
  def go(n: Int, acc: Int): Int = {
    if (n <= 0) acc
    else go(n-1, n*acc )
  }

  go(n, 1)
}

//Ex 2.1
def fib(n: Int):Int = {

  @tailrec
  def go(index: Int, acc: (Int, Int)): Int = {
    if(index == 0) {
      acc._1
    } else {
      go(index - 1, (acc._2, acc._1 + acc._2))
    }

  }

  go(n, (0,1))
}

def partial[A,B,C](a: A, f: (A,B) => C): B => C =
  (b: B) => f(a, b)

//Ex 2.3
def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
  a => partial(a, f)
}

//Ex 2.4
def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
  (a, b) => f(a)(b)
}

//Ex 2.5
def compose[A,B,C](f: B => C, g: A => B): A => C = {
  a => f(g(a))
}
