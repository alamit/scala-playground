import scala.annotation.tailrec

object A {
  println("hey")
}

class Rational(x: Int, y: Int) {

  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x, 1)

  private val factor = gcdAbs(x, y)

  val numerator: Int = x / factor
  val denominator: Int = y / factor

  def unary_- = new Rational(-numerator, denominator)

  def unary_~ = new Rational(denominator, numerator)

  def +(that: Rational): Rational = new Rational(
    numerator * that.denominator + that.numerator * denominator,
    denominator * that.denominator
  )

  def -(that: Rational): Rational = this + -that

  def *(that: Rational): Rational = new Rational(
    numerator * that.numerator,
    denominator * that.denominator
  )

  def /(that: Rational): Rational = this * ~that

  def ==(that: Rational): Boolean =
    // Equality only guaranteed by the fact that all Rationals are simplified at
    // construction
    numerator == that.numerator && denominator == that.denominator

  def !=(that: Rational): Boolean = !(this == that)

  def <(that: Rational): Boolean =
    numerator * that.denominator < that.numerator * denominator

  def <=(that: Rational): Boolean = this == that || this < that

  def >(that: Rational): Boolean = that < this

  def >=(that: Rational): Boolean = that <= this

  def max(that: Rational): Rational = if(this >= that) this else that

  def min(that: Rational): Rational = if(this <= that) this else that

  override def toString: String =
    if(denominator == 1) numerator.toString
    else numerator + "/" + denominator

  @tailrec
  private def gcdAbs(a: Int, b: Int): Int = {
    val x = math.abs(a)
    val y = math.abs(b)
    y match {
      case 0 => x
      case _ => gcdAbs(y, x % y)
    }
  }

}

val x = new Rational(1, 2)
val y = new Rational(2, 3)

val testAddition = x + y == new Rational(7, 6)

val testNegation = -x == new Rational(-1, 2)
val testSubtraction = x - y == new Rational(-1, 6)

val testProduct = x * y == new Rational(1, 3)

val testInverse = ~x == new Rational(2, 1)
val testDivision = x / y == new Rational(3, 4)

val testEquals = new Rational(1,5) == new Rational(1, 5)
val testInequals = x != y
val testLess = x < y
val testLessOrEquals = x <= y && new Rational(1, 5) <= new Rational(1, 5)
val testMore = y > x
val testMoreOrEquals = y >= x && new Rational(1, 5) >= new Rational(1, 5)

val testMax = (x max y) == y
val testMin = (x min y) == x