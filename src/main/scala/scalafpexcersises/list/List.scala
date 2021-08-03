package scalafpexcersises.list

import scala.annotation.tailrec

sealed trait List[+A] {

  def flatMap[B](f: A => List[B]): List[B] = {
    this.foldRight(Nil:List[B])((x,y) => {
      y.appendL(f(x))
    })
  }

  def foldRight[B](z: B)(f: (A, B) => B): B =
    this match {
      case Nil => z
      case Cons(x, xs) => {
        f(x, xs.foldRight(z)(f))
      }
    }

  def foldLeft[B](z: B)(f: (B, A) => B): B = {

    @tailrec
    def loop(left: List[A], acc: B): B = {
      left match {
        case Nil => acc
        case Cons(head, tail) => loop(tail, f(acc, head))
      }
    }
    loop(this, z)
  }

  def appendL[B >: A](l2: List[B]): List[B] = {
    reverse.foldLeft(l2)((b,a) => {
      Cons(a, b)
    })
  }

  def reverse:List[A] = {
    this.foldLeft(Nil:List[A])((x,y) => Cons(y,x))
  }
}


case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def empty[A]():List[A] = Nil.asInstanceOf[List[A]]

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, Nil:List[B])((x,y) => {
      appendL(f(x), y)
    })
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => {
        f(x, foldRight(xs, z)(f))
      }
    }

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

  def appendL[A](l1: List[A], l2: List[A]): List[A] = {
    foldLeft(reverse(l1),l2)((x,y) => Cons(y,x))
  }

  def reverse[A](as: List[A]):List[A] = {
    foldLeft(as, Nil:List[A])((x,y) => Cons(y,x))
  }
}