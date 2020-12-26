import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

//3.1
val x = List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) =>x+y
  case Cons(h, t) => h + List.sum(t)
  case _ => 101
}
//3.2
def tail[A](l: List[A]): List[A] = l match {
  case Cons(_, tail) => tail
  case _ => Nil
}

tail(List())

//3.3
def setHead[A](a:A, list: List[A]): List[A] = list match {
  case Cons(_, tail) => Cons(a, tail)
  case _ => Cons(a, Nil)
}

setHead(1, List(2,3))

//3.4
def drop[A](l: List[A], n: Int): List[A] =
  (l, n) match {
    case (l, i) if i <= 0 => l
    case (Cons(_, tail), i) if n > 0 => drop(tail, n - 1)
    case (Nil, _)         => Nil
}

drop(List(1,2,3,4), 2)

//3.5
def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
  l match {
    case Cons(head, tail) if f(head) => dropWhile(tail, f)
    case _ => l
  }
}

dropWhile[Int](List[Int](1,2,3,4,5,1,2,3), _ < 3)

//3.6
def init[A](l: List[A]): List[A] = {
  l match {
    case Cons(_, Nil) => Nil
    case Cons(h, t)   => Cons(h, init(t))
    case _            => error("init: list is empty")
  }
}

def dropWhile2[A](as: List[A])(f: A => Boolean): List[A] =
  as match {
    case Cons(h,t) if f(h) => dropWhile2(t)(f)
    case _ => as
  }

dropWhile2(List(1,2,3,4))(_ < 3)


def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
  as match {
    case Nil => z
    case Cons(x, xs) => {
      f(x, foldRight(xs, z)(f))
    }
  }

foldRight(List(1,2,3,4,5),0)(_+_)

//3.7
def product[A, B >: A](xs: List[A])(implicit num: Numeric[B]): B = {

  def continue(x: B, acc: => B): B = {
    if (x == num.zero) {
      x
    } else {
      acc
    }
  }

  def foldRight[A, B](xs: List[A])(continue: (A, => B) => B, z: B)(f: (A, B) => B): B = {
    xs match {
      case Nil        => z
      case Cons(h, t) => continue(h, f(h, foldRight(t)(continue, z)(f)))
    }
  }

  foldRight(xs)(continue, num.one)(num.times)
}

//3.8
foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))

//3.9
def length[A](xs: List[A]): Int = {
  foldRight(xs, 0)((_, y) => y + 1)
}

//3.10
def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {

  @tailrec
  def loop(left: List[A], acc: B): B = {
    left match {
      case Nil => acc
      case Cons(head, tail) => loop(tail, f(acc, head))
    }
  }
  loop(as, z)
}

foldLeft(List(1,2,3,4), 0)(_+_)

//3.11
def sum(as: List[Int]): Int = {
  foldLeft(as, 0)(_+_)
}

def product(as: List[Int]): Int = {
  foldLeft(as, 1)(_*_)
}

//3.12
def reverse[A](as: List[A]):List[A] = {
  foldLeft(as, Nil:List[A])((x,y) => Cons(y,x))
}

reverse(List(1,2,3))

//3.14
def appendL[A](l1: List[A], l2: List[A]): List[A] = {
  foldLeft(reverse(l1),l2)((x,y) => Cons(y,x))
}

appendL(List(1,2,3), List(4,5,6))

//3.16

def add1(xs: List[Int]): List[Int] = {
  foldRight(xs, Nil: List[Int])((x,y) => Cons(x+1, y))
}

add1(List(1,2,3))

//3.17
def doubleToString(xs: List[Int]): List[String] = {
  foldRight(xs, Nil: List[String])((x,y) => Cons(x.toString+"/d", y))
}

doubleToString(List(1,2,3))

//3.18
def map[A,B](as: List[A])(f: A => B): List[B] = {
  foldRight(as, Nil: List[B])((x,y) => Cons(f(x), y))
}

//3.19
def filter[A](as: List[A])(f: A => Boolean): List[A] = {
  foldRight(as, Nil: List[A])((x, y) => {
    if(f(x)) {
      Cons(x,y)
    } else{
      y
    }
  })
}

//3.20
def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
  foldRight(as, Nil:List[B])((x,y) => {
    appendL(f(x), y)
  })
}

flatMap(List(1,2,4))(i => List(i,i))

//3.21
def filterFM[A](as: List[A])(f: A => Boolean): List[A] = {
  flatMap(as)(a => if(f(a)) List(a) else Nil)
}

filterFM(List(1,2,3,4))(_ < 3)

//3.22
def add(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
  case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, add(xs, ys))
  case _                          => Nil
}

def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A,B) => C): List[C] = (as, bs) match {
  case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  case _ => Nil
}

zipWith(List(1,2,4), List("a","b","c"))(_+_)


sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

val exampleTree = Branch[Int](
  Branch(Leaf(1),
    Leaf(2)),
  Leaf(4))

//3.25
def sizeT[A](t: Tree[A]): Int = {
  t match {
    case Leaf(_) => 1
    case Branch(left, right) => sizeT(left) + sizeT(right)
  }
}

sizeT(exampleTree)

//3.26
def maximum(tree: Tree[Int]): Int = {
  tree match {
    case Leaf(n) => n
    case Branch(l, r) => maximum(l) max maximum(r)
  }
}

//3.27
def depth[A](t: Tree[A]): Int = t match {
  case Leaf(_)      => 0
  case Branch(l, r) => 1 + (depth(l) max depth(r))
}

//3.28

def map[A, B](t:Tree[A])(f: A=>B): Tree[B] = {
  t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }
}

map(exampleTree)(_+2)

//3.29
def foldT[A, B](t: Tree[A])(f: (A) => B)(g: (B, B) => B): B = t match {
  case Leaf(n) => f(n)
  case Branch(l, r) =>
    val v1 = foldT(l)(f)(g)
    val v2 = foldT(r)(f)(g)
    g(v1, v2)
}

def size[A](t: Tree[A]): Int = {
  foldT[A, Int](t)((x) => 1)((l, r) => l + r)
}

def depthF[A](t: Tree[A]): Int = {
  foldT[A, Int](t)((x) => 0)((l, r) => 1 + math.max(l, r))
}

def maximum[A](t: Tree[A])(implicit ord: Ordering[A]): A = {
  foldT[A, A](t)(v => v)((l, r) => ord.max(l, r))
}

def map[A, B](t: Tree[A], f: A => B): Tree[B] = {
  foldT[A, Tree[B]](t)(v => Leaf(f(v)))((l, r) => Branch(l, r))
}




