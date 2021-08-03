package scalafpexcersises.io

import scalafpexcersises.io.io.IO
import scalafpexcersises.io.io.IO._

object Runner {

  def main(args: Array[String]): Unit = {
    val p = IO.forever(printLine("Still going..."))
    println(p)
  }

}
