import common._

def scanLeftSeg[A](input: Array[A],
                   from: Int,
                   to: Int,
                   a0: A,
                   f: (A, A) => A,
                   output: Array[A]) = {
  var i = 0

  output(0) = a0

  while (i < input.length) {
    output(i + 1) = f(output(i), input(i))
    i += 1
  }
}

def reduceSeg1[A](input: Array[A],
                  from: Int,
                  to: Int,
                  a0: A,
                  f: (A, A) => A): A = {
  var a = a0
  var i = from
  while (i < to) {
    a = f(a, input(i))
    i += 1
  }
  a
}

sealed abstract class TreeRes[A] {
  val res: A
}

case class LeafRes[A](from: Int,
                      to: Int,
                      override val res: A) extends TreeRes[A]

case class NodeRes[A](l: TreeRes[A],
                      override val res: A,
                      r: TreeRes[A]) extends TreeRes[A]

val threshold = 5

def upsweep[A](input: Array[A],
               from: Int,
               to: Int,
               f: (A, A) => A): TreeRes[A] = {
  if (from - to < threshold) {
    LeafRes(from, to, reduceSeg1(input, from + 1, to, input(from), f))
  } else {
    val mid = from + (to - from) / 2
    val (tL, tR) = parallel(upsweep(input, from, mid, f),
      upsweep(input, mid, to, f))
    NodeRes(tL, f(tL.res, tR.res), tR)
  }
}

def downsweep[A](input: Array[A],
                 t: TreeRes[A],
                 a0: A,
                 f: (A, A) => A,
                 output: Array[A]): Unit = t match {
  case LeafRes(from, to, res) => scanLeftSeg(input, from, to, a0, f, output)
  case NodeRes(l, res, r) => {
    parallel(downsweep(input, l, a0, f, output),
      downsweep(input, r, f(a0, l.res), f, output))
  }
}

def scanLeft[A](input: Array[A], a0: A, f: (A, A) => A, output: Array[A]) = {
  val t = upsweep(input, 0, input.length, f)
  downsweep(input, t, a0, f, output)
  output(0) = a0
}


val in = Array(1, 2, 3, 4)
val out = new Array[Int](5)

// Seq
scanLeftSeg(in, 0, in.length, 100, (x: Int, y: Int) => x + y, out)
out

// Par
scanLeft(in, 100, (x: Int, y: Int) => x + y, out)
out
