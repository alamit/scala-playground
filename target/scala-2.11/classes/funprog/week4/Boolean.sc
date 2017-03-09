abstract class Boolean {
  def ifThenElse[T] (t: => T, e: => T): T

  def &&(x: => Boolean): Boolean = ifThenElse(x, False)
  def ||(x: => Boolean): Boolean = ifThenElse(True, x)

  def unary_! : Boolean = ifThenElse(False, True)

  def ==(x: Boolean): Boolean = ifThenElse(x, !x)
  def !=(x: Boolean): Boolean = ifThenElse(!x, x)

  def <(x: Boolean): Boolean = ifThenElse(False, x)
}

object True extends Boolean {
  override def ifThenElse[T](t: => T, e: => T): T = t

  override def toString: String = "True"
}

object False extends Boolean {
  override def ifThenElse[T](t: => T, e: => T): T = e

  override def toString: String = "False"
}

True < False
False < True
False < False
True < True