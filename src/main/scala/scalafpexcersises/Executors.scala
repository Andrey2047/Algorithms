package scalafpexcersises;

object Executors {
  val cores = Runtime.getRuntime.availableProcessors

  lazy val fixedThreadPool: ExecutorService = E.newFixedThreadPool(1)

  lazy val singleThreadPool: ExecutorService = E.newSingleThreadExecutor()
}
