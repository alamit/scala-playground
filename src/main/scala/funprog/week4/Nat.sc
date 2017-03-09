abstract class Nat {
  def isZero: Boolean

  def predecessor: Nat

  def successor: Nat

  def +(that: Nat): Nat

  def -(that: Nat): Nat

  def toInt(acc: Int): Int

  override def toString: String = {
    toInt(0).toString
  }

}

object Zero extends Nat {
  override def isZero: Boolean = true

  override def predecessor: Nat = throw new NoSuchElementException

  override def successor: Nat = new Succ(Zero)

  override def +(that: Nat): Nat = that

  override def -(that: Nat): Nat = that match {
    case Zero => Zero
    case _ => throw new NoSuchElementException
  }

  override def toInt(acc: Int): Int = acc
}

class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false

  override def predecessor: Nat = n

  override def successor: Nat = new Succ(this)

  override def +(that: Nat): Nat = predecessor + that.successor

  override def -(that: Nat): Nat = that match {
    case Zero => this
    case _ => predecessor - that.predecessor
  }

  override def toInt(acc: Int): Int = predecessor.toInt(acc + 1)
}

val three = new Succ(new Succ(new Succ(Zero)))
val two = new Succ(new Succ(Zero))

two + three
three - two
try {
  two - three
} catch {
  case _ : NoSuchElementException => println("Good exception thrown")
}
two + Zero

