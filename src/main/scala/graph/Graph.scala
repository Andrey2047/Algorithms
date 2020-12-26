package graph

abstract class GraphBase[T, U] {
  case class Edge(n1: Node, n2: Node, value: U) {
    def toTuple = (n1.value, n2.value, value)
  }
  case class Node(value: T) {
    var adj: List[Edge] = Nil
    // neighbors are all nodes adjacent to this node.
    def neighbors: List[Node] = adj.map(edgeTarget(_, this).get)
  }

  var nodes: Map[T, Node] = Map()
  var edges: List[Edge] = Nil

  // If the edge E connects N to another node, returns the other node,
  // otherwise returns None.
  def edgeTarget(e: Edge, n: Node): Option[Node]

  override def equals(o: Any) = o match {
    case g: GraphBase[_,_] => nodes.keys.toList.diff(g.nodes.keys.toList) == Nil &&
      edges.map(_.toTuple).diff(g.edges.map(_.toTuple)) == Nil
    case _ => false
  }
  def addNode(value: T) = {
    val n = new Node(value)
    nodes = Map(value -> n) ++ nodes
    n
  }

  def edgeConnectsToGraph[T,U](e: Edge, nodes: List[Node]): Boolean =
    !(nodes.contains(e.n1) == nodes.contains(e.n2))  // xor

  //Write a method named findPaths to find acyclic paths from
  // one node to another in a graph. The method should return all paths.
  def findPaths(sourceNode: T, destNode:T): List[List[T]] = {
    def loop(currentNode: Node, currentPath: List[T]): List[List[T]] = {
      if(currentNode.value == destNode) List(currentPath)
      else currentNode.adj.map(edgeTarget(_, currentNode).get)
        .filter(n => !currentPath.contains(n.value)).flatMap(n => loop(n,  n.value :: currentPath))
    }

    loop(nodes(sourceNode), List(sourceNode))
  }

  //Write a method named findCycles to find closed paths (cycles) starting at
  // a given node in a graph. The method should return all cycles.
  def findCycles(nodeName: T):List[List[T]] = {
    val node = nodes(nodeName)
    node.adj.map(edgeTarget(_, node).get.value).flatMap(findPaths(_, node.value)).map(nodeName :: _).filter(_.size > 3)
  }

  // Write a method minimalSpanningTree to construct the minimal spanning tree
  // of a given labeled graph.  Hint: Use Prim's Algorithm.  A small
  // modification of the solution of P83 does the trick.  The data of the
  // example graph to the right can be found below.
  def findMinimumSpanningTree()(implicit f: U => Ordered[U]): Graph[T,U] = {

   def loop(treeE: List[Edge], treeN: List[Node], nodesLeft: List[Node]): Graph[T,U] = {
     if (nodesLeft == Nil) {
       Graph.termLabel(nodes.keys.toList, treeE.map(_.toTuple))
     } else {
       val minEdge = treeN.flatMap(_.adj).filterNot(e =>
         treeN.contains(e.n1) && treeN.contains(e.n2)
       ).reduceLeft((r, e) => {
         if (r.value > e.value) e else r
       })

       val newVert = if (treeN.contains(minEdge.n1)) {
         minEdge.n2
       } else {
         minEdge.n1
       }

       loop(minEdge :: treeE, newVert :: treeN, nodesLeft filterNot (_ == newVert))

     }
   }
    loop(List(), List(nodes.values.head), nodes.values.tail.toList)
  }
}

class Graph[T, U] extends GraphBase[T, U] {
  override def equals(o: Any) = o match {
    case g: Graph[_,_] => super.equals(g)
    case _ => false
  }

  def edgeTarget(e: Edge, n: Node): Option[Node] =
    if (e.n1 == n) Some(e.n2)
    else if (e.n2 == n) Some(e.n1)
    else None

  def addEdge(n1: T, n2: T, value: U) = {
    val e = new Edge(nodes(n1), nodes(n2), value)
    edges = e :: edges
    nodes(n1).adj = e :: nodes(n1).adj
    nodes(n2).adj = e :: nodes(n2).adj
  }
}

class Digraph[T, U] extends GraphBase[T, U] {
  override def equals(o: Any) = o match {
    case g: Digraph[_,_] => super.equals(g)
    case _ => false
  }

  def edgeTarget(e: Edge, n: Node): Option[Node] =
    if (e.n1 == n) Some(e.n2)
    else None

  def addArc(source: T, dest: T, value: U) = {
    val e = new Edge(nodes(source), nodes(dest), value)
    edges = e :: edges
    nodes(source).adj = e :: nodes(source).adj
  }
}

abstract class GraphObjBase {
  type GraphClass[T, U]
  def addLabel[T](edges: List[(T, T)]) =
    edges.map(v => (v._1, v._2, ()))
  def term[T](nodes: List[T], edges: List[(T,T)]) =
    termLabel(nodes, addLabel(edges))
  def termLabel[T, U](nodes: List[T], edges: List[(T,T,U)]): GraphClass[T, U]
  def addAdjacentLabel[T](nodes: List[(T, List[T])]) =
    nodes.map(a => (a._1, a._2.map((_, ()))))
  def adjacent[T](nodes: List[(T, List[T])]) =
    adjacentLabel(addAdjacentLabel(nodes))
  def adjacentLabel[T, U](nodes: List[(T, List[(T,U)])]): GraphClass[T, U]
}

object Graph extends GraphObjBase {
  type GraphClass[T, U] = Graph[T, U]

  def termLabel[T, U](nodes: List[T], edges: List[(T,T,U)]) = {
    val g = new Graph[T, U]
    nodes.map(g.addNode)
    edges.foreach(v => g.addEdge(v._1, v._2, v._3))
    g
  }

  def adjacentLabel[T, U](nodes: List[(T, List[(T,U)])]) = {
    val g = new Graph[T, U]
    for ((v, a) <- nodes) g.addNode(v)
    for ((n1, a) <- nodes; (n2, l) <- a) {
      if (!g.nodes(n1).neighbors.contains(g.nodes(n2)))
        g.addEdge(n1, n2, l)
    }
    g
  }
}

//Directed graph
object Digraph extends GraphObjBase {
  type GraphClass[T, U] = Digraph[T, U]

  def termLabel[T, U](nodes: List[T], edges: List[(T,T,U)]) = {
    val g = new Digraph[T, U]
    nodes.map(g.addNode)
    edges.map(v => g.addArc(v._1, v._2, v._3))
    g
  }
  def adjacentLabel[T, U](nodes: List[(T, List[(T,U)])]) = {
    val g = new Digraph[T, U]
    for ((n, a) <- nodes) g.addNode(n)
    for ((s, a) <- nodes; (d, l) <- a) g.addArc(s, d, l)
    g
  }
}
