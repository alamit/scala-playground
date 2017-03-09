import scala.collection.GenSeq

def largestPalindrome(xs: GenSeq[Int]): Int = {
  xs.aggregate(Int.MinValue)(
    (largest, n) =>
      if(n > largest && n.toString == n.toString.reverse)
        n
      else
        largest
    , math.max
  )
}

val array = (0 until 1000000).toArray