package graph

import scala.language.implicitConversions

object Conversion {

  class GraphConversion(str: String) {
    def toGraph: Graph[String, Int] = {
      val g = new Graph[String, Int]

      val tokens = str.replace(" ", "").split(",")
      tokens.flatMap(_.replace("-","").replaceAll("\\d", "").replaceAll("/","").toCharArray).toSet[Char]
        .foreach(c => g.addNode(c.toString))

      tokens.filter(_.contains("-")).foreach(x => {
        val s = x.split("-")
        val (n2, value) = if(s(1).contains("/")) {
          val k = s(1).split("/")
          (k(0), k(1))
        } else (s(1), "")
        g.addEdge(s(0), n2, Integer.parseInt(value))
      })
      g
    }
  }


  implicit def stringToGraph(str: String): GraphConversion = {
    new GraphConversion(str)
  }
}
