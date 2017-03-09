def init[T](xs: List[T]): List[T] = xs match {
  case Nil => throw new NoSuchElementException
  case x :: Nil => Nil
  case y :: ys => y :: init(ys)
}

def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case Nil => ys
  case z :: zs => z :: concat(zs, ys)
}

def reverse[T](xs: List[T]): List[T] = xs match {
  case Nil => xs
  case y :: ys => reverse(ys) ++ List(y)
}

def removeAt[T](m: Int, xs: List[T]): List[T] = {
  def removeAcc(xs: List[T], counter: Int, pre: List[T]): List[T] = xs match {
    case Nil => xs
    case y :: ys =>
      if(counter == m) pre ++ ys
      else removeAcc(ys, counter + 1, pre ++ List(y))
  }
  removeAcc(xs, 0, Nil)
}

def flatten(xs: List[Any]): List[Any] = xs match {
  case Nil => xs
  case y :: ys => y match {
    case zs: List[Any] => flatten(zs ++ ys)
    case _ => y :: flatten(ys)
  }
}

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case y :: _ => {
    val (left, right) = xs span (_ == y)
    left :: pack(right)
  }
}

def encode[T](xs: List[T]): List[(T, Int)] = pack(xs) map (x => (x.head, x.length))

val list = List("a", "b", "c", "d")
val removed = removeAt(1, list)
val reverse: List[String] = reverse(list)
val concat: List[String] = concat(list, List("e", "f", "g", "h"))
val init: List[String] = init(list)
val flatten: List[Any] = flatten(List(list, removed, reverse))

val list2 = List("a", "a", "c", "a", "c", "d", "e", "a", "g", "e", "g", "g")
val packed = pack(list2)
val encoded = encode(list2)
