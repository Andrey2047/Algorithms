package scalafp.io.par

import java.util.concurrent.Executors

import scalafp.paralelism.Par

object Runner {

  def main(args: Array[String]): Unit = {
      val s = Suspend(Par.lazyUnit({
        println(Thread.currentThread())
        12
      }))

      println(Thread.currentThread())

      val exs = Executors.newFixedThreadPool(1)

      println(Async.run(s)(exs).get())

  }

}
