import scala.annotation.tailrec

def streamRange(lo: Int, hi: Int): Stream[Int] = {
  @tailrec
  def streamRangeAcc(lo: Int, hi: Int, acc: Stream[Int]): Stream[Int] = {
    if (hi < lo) acc
    else streamRangeAcc(lo, hi - 1, Stream.cons(hi, acc))
  }

  streamRangeAcc(lo, hi, Stream.empty)
}

def isPrime(n: Int) = (2 until n) forall (n % _ != 0)

streamRange(1000, 10000).filter(isPrime)(1060)


def from(n: Int): Stream[Int] = n #:: from(n + 1)

val nats = from(0)

val m4s = nats map (_ * 4)

m4s take 100 toList

def sieve(s: Stream[Int]): Stream[Int] =
  s.head #:: sieve(s.tail filter (_ % s.head != 0))

val primes = sieve(from(2))

primes take 100 toList

def sqrtStream(x: Double): Stream[Double] = {
  def improve(guess: Double) = (guess + x / guess) / 2
  lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
  guesses
}

sqrtStream(4) take 10 toList