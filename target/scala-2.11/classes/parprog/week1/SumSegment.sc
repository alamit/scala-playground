import common._

def sumSegment(a: Array[Int], p: Double, s: Int, t: Int): Int =
  a.slice(s, t).map(x => math.pow(math.abs(x), p)).sum.toInt

def pNormTwoPart(a: Array[Int], p: Double): Int = {
  val m = a.length / 2

  val (sum1, sum2) = parallel(sumSegment(a, p, 0, m),
    sumSegment(a, p, m, a.length))

  math.pow(sum1 + sum2, 1.0/p).toInt
}

pNormTwoPart(Array(1, 1), 2.0)