def combinations(m: Int, n: Int) =
  (1 to m) flatMap (x => (1 to n) map (y => (x, y)))

def combinationsExcl(n: Int) =
  (1 until n) flatMap (i => (1 until i) map (j => (i, j)))

combinations(2, 3)

def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys).map{case (x, y) => x * y}.sum

scalarProduct(Vector(1, 2, 3), Vector(3, 4, 5))

def isPrime(n: Int): Boolean =
  (2 until n) forall (n % _ != 0)

isPrime(2)
isPrime(3)
isPrime(154)
isPrime(17)

def pairsWithPrimeSum(n: Int) = combinationsExcl(n) filter {
  case (i, j) => isPrime(i + j)
}

pairsWithPrimeSum(7)

def pairsWithPrimeSumFor(n: Int) = for {
  i <- 1 until n
  j <- 1 until i
  if isPrime(i + j)
} yield (i, j)

pairsWithPrimeSumFor(7)

def scalarProductFor(xs: Vector[Double], ys: Vector[Double]): Double =
  (for((x, y) <- xs zip ys) yield x * y).sum

scalarProductFor(Vector(1, 2, 3), Vector(3, 4, 5))
