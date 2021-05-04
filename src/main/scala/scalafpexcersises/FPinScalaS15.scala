package scalafp

import scalafp.io.io.IO

import scala.io.Source

class FPinScalaS15 {


  def linesGt40k(filename: String): IO[Boolean] = IO {
    val src = Source.fromFile(filename)
    try {
      var count = 0
      val lines: Iterator[String] = src.getLines
      while (count <= 40000 && lines.hasNext) {
        lines.next
        count += 1
      }
      count > 40000
    }
    finally src.close
  }

}
