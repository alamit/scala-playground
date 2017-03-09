import common._
import org.scalameter._

/**
  * Very straightforward sequential implementation, would need more
  * optimization in order to make the measurements relevants
  */
def quickSort(xs: Array[Int], from: Int, until: Int): Unit = {
  xs.slice(from, until).sorted
}

def parMergeSort(xs: Array[Int], maxDepth: Int): Unit = {

  val ys = new Array[Int](xs.length)

  def merge(src: Array[Int], dst: Array[Int], from: Int, mid: Int, until: Int) = {
    var (l, r) = (from, until)
    var i = from

    while(l < mid && r < until) {

      while(l < mid && src(l) <= src(r)) {
        dst(i) = src(l)
        i += 1
        l += 1
      }

      while(r < until && src(r) <= src(l)) {
        dst(i) = src(r)
        i += 1
        r += 1
      }
    }

    while(l < mid) {
      dst(i) = src(l)
      i += 1
      l += 1
    }

    while(r < until) {
      dst(i) = src(r)
      i += 1
      r += 1
    }
  }

  def sort(from: Int, until: Int, depth: Int): Unit = {
    if (depth == maxDepth)
      quickSort(xs, from, until)
    else {
      val mid = (from + until) / 2
      parallel(sort(from, mid, depth + 1), sort(mid, until, depth + 1))

      val flip = (maxDepth - depth) % 2 == 0
      val src = if (flip) ys else xs
      val dst = if (flip) xs else ys

      merge(src, dst, from, mid, until)
    }
  }
}

val standardConfig = config(
  Key.exec.minWarmupRuns -> 20,
  Key.exec.maxWarmupRuns -> 60,
  Key.exec.benchRuns -> 60,
  Key.verbose -> true
) withWarmer(new Warmer.Default)

def initialize(xs: Array[Int]) {
  var i = 0
  while (i < xs.length) {
    xs(i) = i % 100
    i += 1
  }
}

val length = 10000000
val maxDepth = 7
val xs = new Array[Int](length)

val seqtime = standardConfig setUp {
  _ => initialize(xs)
} measure {
  quickSort(xs, 0, xs.length)
}

println(s"sequential sum time: $seqtime ms")

val partime = standardConfig setUp {
  _ => initialize(xs)
} measure {
  parMergeSort(xs, maxDepth)
}

println(s"fork/join time: $partime ms")
println("speedup:" + seqtime.value / partime.value)


