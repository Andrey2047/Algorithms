package trampoling

import scalafpexcersises.io.tailRec.{ Suspend, TailRec, Return }

object QuickSort {

  def main(args: Array[String]): Unit = {
    val a = List.fill(3)((Math.random()*100000).toInt)

    val st1 = System.currentTimeMillis()
    quickSort(a)
    println(s"time1 - ${st1 - System.currentTimeMillis()}")

    val st2 = System.currentTimeMillis()
    val value = quickSortT(a)
    TailRec.run(value)
    println(s"time2 - ${st2 - System.currentTimeMillis()}")

  }

  def quickSort(xs: List[Int]): List[Int] = {
    xs match {
      case Nil => Nil
      case x :: tail =>
        val (left, right) = tail.partition(_ < x)
        (quickSort(left) :+ x) ::: quickSort(right)
    }
  }

  def quickSortT(xs: List[Int]): TailRec[List[Int]] = {
    xs match {
      case Nil =>
        Return(Nil)
      case x :: tail =>
        val (left, right) = tail.partition(_ < x)
        val f: TailRec[List[Int]] = for {
          ls <- quickSortT(left)
          rs <- quickSortT(right)
        } yield (ls :+ x) ::: rs

        Suspend(() => TailRec.run[List[Int]](f))
    }
  }
}
