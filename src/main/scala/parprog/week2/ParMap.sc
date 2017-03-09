import common._

def mapASegSeq[A, B](input: Array[A],
                     left: Int,
                     right: Int,
                     f: A => B,
                     output: Array[B]): Unit = {
  var i = left
  while(i < right) {
    output(i) = f(input(i))
    i += 1
  }
}

val in = Array(2, 3, 4, 5, 6)
val out = new Array[Int](in.length)

val f = (x: Int) => x * x

mapASegSeq(in, 1, 3, f, out)
out

def mapASegPar[A, B](input: Array[A],
                     f: A => B,
                     output: Array[B],
                     maxDepth: Int): Unit = {

  def map(left: Int, right: Int, depth: Int): Unit = {

    if(depth == maxDepth) mapASegSeq(input, left, right, f, output)

    else {
      val mid = (left + right) / 2
      parallel(map(left, mid, depth + 1), map(mid, right, depth + 1))
    }
  }

  map(0, input.length, 0)

}
