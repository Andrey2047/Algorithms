package scalafp

import scalafp.FPInScalaS7.Par.{ Par, map, unit }

import scala.concurrent.duration.TimeUnit

import java.util.concurrent.{ Callable, TimeUnit, ExecutorService, Future }

object FPInScalaS7 {

  object Par {

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true
      def get(timeout: Long, units: TimeUnit): A = get
      def isCancelled = false
      def cancel(evenIfRunning: Boolean): Boolean = false
    }

    type Par[A] = ExecutorService => Future[A]

    def unit[A](a: => A): Par[A] =  (es: ExecutorService) => UnitFuture(a)

    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

    def fork[A](a: => Par[A]): Par[A] = {
      es: ExecutorService =>
        es.submit(new Callable[A] {
          override def call(): A = a(es).get
        })
    }

    def map[A, B](a: Par[A])(f: A => B): Par[B] = (es: ExecutorService) => {
      val s = a(es)
      UnitFuture(f(s.get))
    }

    //ex 7.5
    def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
      ps.foldRight(Par.unit(List.empty[A]))((a, b) => {
        map2(a, b)((x,y) => x :: y)
      })
    }

    //ex 7.6
    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
      val len = as.length
      if (len <= 1)
        unit(as.filter(f))
      else {
        val (l, r) = as.splitAt(len / 2)
        map2(fork(parFilter(l)(f)), fork(parFilter(r)(f)))(_ ++ _)
      }
    }

    //Ex 7.4
    def asyncF[A,B](f: A => B): A => Par[B] = {
      a => {
        lazyUnit(f(a))
      }
    }

    def map2[A, B, C](p1: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
      val s1 = p1(es)
      val s2 = p2(es)
      UnitFuture(f(s1.get, s2.get))
    }

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
      p(e).get == p2(e).get

    def delay[A](fa: => Par[A]): Par[A] =
      es => fa(es)

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      es =>
        if (run(es)(cond).get) t(es)
        else f(es)

    //ex 7.11
    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
      choices(n(es).get())(es)
    }

    //ex 7.12
    def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = ???

    //ex 7.13
    def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = es => {
      choices(pa(es).get)(es)
    }

    def flatMap[A,B](a: Par[A])(f: A => Par[B]): Par[B] = es => {
      f(a(es).get)(es)
    }

    def join[A](a: Par[Par[A]]): Par[A] = es => {
        val aI = run(es)(a).get
        aI(es)
    }

    def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
      flatMap(a)(x => x)

  }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1) {
      Par.unit(ints.headOption getOrElse 0)
    } else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))((a,b) => {
        println(Thread.currentThread())
        a + b
      })
    }

  def main(args: Array[String]): Unit = {
    map(unit(1))(_ + 1) == unit(2)

    val f: Int => Int = _ + 1
    map(unit(1))(f) == unit(f(1))

    val x = 1
    val id = (x:Int) => x

    //Second law of free theorem:
    map(unit(x))(f) == unit(f(x))

    map(unit(x))(id) == unit(id(x))
    map(unit(x))(id) == unit(x)

    val z = (k: Int) => Par.unit(k)

    Par.fork(z(2)) == z(2)

    println(sum(Array(1,2,3,4,5,6,7))(Executors.fixedThreadPool).get())


  }
}
