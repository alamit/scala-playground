import scala.annotation.tailrec

def sum(f: Int => Int)(a: Int, b: Int): Int = {
  @tailrec
  def sumAcc(a: Int, acc: Int): Int = {
    if (a > b) acc
    else sumAcc(a + 1, acc + f(a))
  }
  sumAcc(a, 0)
}

val testSumId = sum(x => x)(0,3) == 6
val testSumSquare = sum(x => x * x)(0,3) == 14
val testSumNegativeInterval = sum(x => x)(4, 2) == 0
val testSumId2 = sum(x => x)(1, 100) == (100 * 101) / 2
val sumId = sum(x => x)(_,_)

def product(f: Int => Int)(a: Int, b: Int): Int = {

  @tailrec
  def productAcc(a: Int, acc: Int): Int =  {
    if(a > b) acc
    else productAcc(a + 1, acc * f(a))
  }

  productAcc(a, 1)

}

val testProductIdFromZero = product(x => x)(0,3) == 0
val testProductIdFromOne = product(x => x)(1,3) == 6
val testProductSquares = product(x => x * x)(1,3) == 36
val testProductNegativeInterval = product(x => x)(4, 2) == 1

def factorial(n: Int): Int = product(x => x)(1, n)

val testFactorialZero = factorial(0) == 1
val testFactorial = factorial(3) == 6

def mapReduce(operator: (Int, Int) => Int, zero: Int)(transform: Int => Int)(a: Int, b: Int): Int = {
  @tailrec
  def mapReduceAcc(a: Int, acc: Int): Int =  {
    if(a > b) acc
    else mapReduceAcc(a + 1, operator(acc, transform(a)))
  }

  mapReduceAcc(a, zero)
}

val testProduct = mapReduce((x,y) => x * y, 1)(x => x)(1,4) == product(x => x)(1,4)
val testSum = mapReduce((x,y) => x + y, 0)(x => x)(1,4) == sum(x => x)(1,4)