def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length / 2
  n match {
    case 0 => xs
    case _ => {
      val (left, right) = xs splitAt n
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, _) => ys
        case (_, Nil) => xs
        case (x1 :: xs1, y1 :: ys1) =>
          if(ord.lt(x1, y1)) x1 :: merge(xs1, ys)
          else y1 :: merge(xs, ys1)
      }
      merge(msort(left), msort(right))
    }
  }
}

val x = List(4, 5, 1, 7, 0)
val sorted = msort(x)

val strings = List("orange", "banana", "grape", "apple")
val sortedFruits = msort(strings)