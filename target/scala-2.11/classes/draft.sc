

class Node {
  override def toString: String = "Node"
}

abstract class Graph {
  def build: Unit
  build
}

class MyGraph extends Graph {

  var node: Node = _

  override def build: Unit = {
    node = new Node
  }
}

val graph = new MyGraph
graph.node

var x = 0
var y = 0

val length = 33
val numTasks = 32
val step = (length / numTasks)
val rem = length % numTasks

for {
  i <- 0 until length by step
} yield (i, (i + step))


trait A {
  a =>

  val x: a.type

}

0f - 12f

Float.MaxValue + Float.MaxValue
Float.MaxValue + 1f + 1f + 1f