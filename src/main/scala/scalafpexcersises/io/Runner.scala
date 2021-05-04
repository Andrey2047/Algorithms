package scalafp.io

import scalafp.io.io.IO
import scalafp.io.io.IO._

object Runner {

  def main(args: Array[String]): Unit = {
    val p = IO.forever(printLine("Still going..."))
    println(p)
  }

}
