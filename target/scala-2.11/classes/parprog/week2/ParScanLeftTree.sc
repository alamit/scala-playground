import common.parallel

// Classic Tree definition
sealed abstract class Tree[A]
case class Leaf[A](a: A) extends Tree[A]
case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// Tree definition with results
sealed abstract class TreeRes[A]{
  val res: A
}
case class LeafRes[A](override val res: A) extends TreeRes[A]
case class NodeRes[A](l: TreeRes[A],
                      override val res : A,
                      r: TreeRes[A]) extends TreeRes[A]


def reduceRes[A](t: Tree[A], f: (A, A) => A): TreeRes[A] = t match {
  case Leaf(a) => LeafRes(a)
  case Node(l, r) => {
    val (tL, tR) = (reduceRes(l, f), reduceRes(r, f))
    NodeRes(tL, f(tL.res, tR.res), tR)
  }
}

def upsweep[A](t: Tree[A], f: (A, A) => A): TreeRes[A] = t match {
  case Leaf(a) => LeafRes(a)
  case Node(l, r) => {
    val (tL, tR) = parallel(upsweep(l, f), upsweep(r, f))
    NodeRes(tL, f(tL.res, tR.res), tR)
  }
}

def downsweep[A](t: TreeRes[A], a0: A, f: (A, A) => A): Tree[A] = t match {
  case LeafRes(a) => Leaf(f(a0, a))
  case NodeRes(l, res, r) => {
    val (tL, tR) = parallel(downsweep(l, a0, f), downsweep(r, f(a0, l.res), f))
    Node(tL, tR)
  }
}

def prepend[A](x: A, t: Tree[A]): Tree[A] = t match {
  case Leaf(v) => Node(Leaf(x), Leaf(v))
  case Node(l, r) => Node(prepend(x, l), r)
}

def parScanLeft[A](t: Tree[A], a0: A, f: (A, A) => A): Tree[A = {
  val tRes = upsweep(t, f)
  val scan1 = downsweep(tRes, a0, f)
  prepend(a0, scan1)
}
