def squareList(xs: List[Int]): List[Int] = xs match {
  case Nil => xs
  case y :: ys => (y * y) :: squareList(ys)
}

def squareListMap(xs: List[Int]): List[Int] = xs map (x => x * x)

val x = List(1, 2, 3, 4 ,5)
val squareRec = squareList(x)
val squareMap = squareListMap(x)