def queens(n: Int): Set[List[Int]] = {
  def placeQueens(k: Int): Set[List[Int]] =
    if(k == 0) Set(Nil)
    else for {
      queens <- placeQueens(k - 1)
      col <- 0 until n
      if isSafe(col, queens)
    } yield col :: queens // Result is reversed more efficient not to reverse here
  placeQueens(n)
}

def isSafe(col: Int, queens: List[Int]): Boolean = {
  val row = queens.length
  // Following could be done with a for, maybe clearer.
  val queensWithRow = (row - 1 to 0 by -1) zip queens // gives indexes of queens
  queensWithRow forall {
    case (r, c) => c != col && math.abs(col - c) != row - r
  }
}

def show(queens: List[Int]): String = {
  val lines = for (col <- queens.reverse)
    yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
  "\n" + (lines mkString "\n")
}

(queens(4) map show).mkString("\n")