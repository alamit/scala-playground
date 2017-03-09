import fungraph.{Node, RandomGraph}

def ramsey(g: Set[Node[Int]], a: Set[Node[Int]], b: Set[Node[Int]]):
  (Set[Node[Int]], Set[Node[Int]]) = {
  if(g.isEmpty) {
    (a, b)
  } else {
    val node = g.head
    if(node.degree > 0.5 * g.tail.size) {
      ramsey(g.tail.filterNot(node.isConnected), a + node, b)
    } else {
      ramsey(g.tail.filter(node.isConnected), a, b + node)
    }
  }
}

val testList = for(i <- 0 until 1000) yield
  (ramsey(RandomGraph(i).nodes, Set.empty, Set.empty), i)

val testResult = testList.map {
  case ((a: Set[Node[Int]], b: Set[Node[Int]]), i: Int) => {
    val bound = 0.5 * math.log(i)
    a.size >= bound || b.size >= bound
  }
}