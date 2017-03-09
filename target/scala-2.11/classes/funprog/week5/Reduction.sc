import scala.annotation.tailrec

def sum(xs: List[Int]): Int = xs reduceLeft(_ + _)

sum(List(1, 2, 3))

@tailrec
def foldLeft[T, U](xs: List[T])(z: U)(op: (U, T) => U): U = xs match {
  case Nil => z
  case y :: ys => foldLeft(ys)(op(z, y))(op)
}

def reduceLeft[T](xs: List[T])(op: (T, T) => T): T = xs match {
  case Nil => throw new IllegalArgumentException("reduceLeft on Nil!")
  case y :: ys => foldLeft(ys)(y)(op)
}

def foldRight[T, U](xs: List[T])(z: U)(op: (T, U) => U): U = xs match {
  case Nil => z
  case y :: ys => op(y, foldRight(ys)(z)(op))
}

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  foldRight(xs)(List[U]())(f(_) :: _)

def lengthFun[T](xs: List[T]): Int =
  foldRight(xs)(0)((t, n) => n + 1)

val x = List(1, 2, 3, 4, 5)
val sum = foldLeft(x)(0)(_ + _)
val product = foldLeft(x)(1)(_ * _)
val sum2 = reduceLeft(x)(_ + _)
val sum3 = foldRight(x)(0)(_ + _)
val lengthFun: Int = lengthFun(x)
val mapFun: List[Char] = mapFun(x, (n: Int) => (n + 64).toChar)