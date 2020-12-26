package graph

import Conversion._

object Test {

  def main(args: Array[String]): Unit = {
//    val g = Graph.term(List('b', 'c', 'd', 'f', 'g', 'h', 'k'),
//      List(('b', 'c'), ('b', 'f'), ('c', 'f'), ('f', 'k'), ('g', 'h')))
//
//
//    val adjGraph = Graph.adjacent(List(('b', List('c', 'f')), ('c', List('b', 'f')), ('d', Nil),
//      ('f', List('b', 'c', 'k')), ('g', List('h')), ('h', List('g')),
//      ('k', List('f'))))
//
//
//    println(adjGraph)
//
//    val s = "a-b, a-g, a-d, b-c, c-d, d-e, e-g".toGraph
//
//
//    println(s.findCycles("a"))


    val exampleGraph = "a-b/7, a-d/5, d-b/9, b-c/8, c-e/5, b-e/7, e-g/9, f-g/11, d-f/6, d-e/15".toGraph

    exampleGraph.findMinimumSpanningTree()




  }

}
