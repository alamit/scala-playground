val romanNumerals = Map("I" -> 1, "V" -> 5, "X" -> 10)

val capitalOfCountry = Map("US" -> "Washington", "CH" -> "Berne")

capitalOfCountry("US")

def showCapital(country: String): String = capitalOfCountry get country match {
  case Some(capital) => capital
  case None => "No data"
}

showCapital("FR")

class Poly(_terms: Map[Int, Double]) {
  def this(bindings: (Int, Double)*) = this(bindings.toMap)
  val terms = _terms withDefaultValue 0.0
  def +(other: Poly): Poly = new Poly(terms ++ (other.terms map adjust))

  def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = term
    exp -> (coeff + terms(exp))
  }

  override def toString: String =
    (for((exp, coeff) <- terms.toList.sorted.reverse) yield coeff+"x^"+exp) mkString " + "
}

val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))

p1 + p2

