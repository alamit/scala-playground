import scala.annotation.tailrec

def factorial(n: Int): Int = {
  require(n >= 0)
  @tailrec
  def factorialAcc(n: Int, acc: Int): Int = n match {
    case 0 => acc
    case _ => factorialAcc(n - 1, n * acc)
  }
  factorialAcc(n, 1)
}

factorial(3)
factorial(10)
