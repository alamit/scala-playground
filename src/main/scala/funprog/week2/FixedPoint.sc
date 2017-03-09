import scala.annotation.tailrec

def fixedPoint(f: Double => Double)
              (firstGuess: Double)
              (isCloseEnough: (Double, Double) => Boolean) = {

  @tailrec
  def iterate(guess: Double): Double = {
    val next = f(guess)
    if(isCloseEnough(guess, next)) next
    else iterate(next)
  }

  iterate(firstGuess)
}

def averageDamp(f: Double => Double)(x: Double): Double = (x + f(x)) / 2

def defaultIsCloseEnough(epsilon: Double)(x: Double, y: Double): Boolean =
  math.abs(x - y) / x < epsilon

def defaultFixedPoint(f: Double => Double)(firstGuess: Double): Double =
  fixedPoint(f)(firstGuess)(defaultIsCloseEnough(0.0001))

val testFixedPoint =
  defaultIsCloseEnough(0.0001)(defaultFixedPoint(x => 1 + x/2)(1), 2.0)

def sqrt(x: Double): Double = defaultFixedPoint(averageDamp(y => x / y))(1)

sqrt(2)