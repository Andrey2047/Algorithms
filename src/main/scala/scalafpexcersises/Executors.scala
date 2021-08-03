package scalafpexcersises

import java.util.concurrent.ExecutorService

object Executors {
  val cores = Runtime.getRuntime.availableProcessors

  lazy val fixedThreadPool: ExecutorService = java.util.concurrent.Executors.newFixedThreadPool(1)

  lazy val singleThreadPool: ExecutorService = java.util.concurrent.Executors.newSingleThreadExecutor()
}
