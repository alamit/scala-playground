package fungraph

import scala.util.Random

/**
  * Created by adrienlamit on 27/02/17.
  */

case class Node[T](elem: T, connections: Set[Node[T]]){
  override def toString: String = "Node(elem.toString)"
  def degree: Int = connections.size
  def isConnected(node: Node[T]): Boolean = connections.contains(node)
}

case class RandomGraph(n: Int) {
  val nodes = {
    val unConnected = (0 until n) map (Node(_, Set[Node[Int]]()))
    unConnected.map(n => Node(n.elem, unConnected.filter(_ => Random.nextBoolean()).toSet)).toSet
  }
}
