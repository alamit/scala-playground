import scala.annotation.tailrec

def abs(x: Double): Double = if (x < 0) -x else x

def sqrt(x: Double): Double = {

  @tailrec
  def sqrtIter(guess: Double): Double = {

    def isGoodEnough(guess: Double): Boolean = abs(guess * guess - x) / x < 0.001
    // Division by x to adjust our error,
    // prevents non-termination for large numbers, and big error for small ones

    def improve(guess: Double): Double = (guess + x / guess) / 2

    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))
  }

  sqrtIter(1.0)

}


sqrt(2)
sqrt(4)

sqrt(1e-6)
sqrt(1e50) // Non termination
