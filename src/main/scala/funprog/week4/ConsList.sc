import scala.annotation.tailrec

trait List[+T] {
  def isEmpty: Boolean

  def head: T

  def tail: List[T]

  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)

  def tailString: String = tail match {
    case _: Cons[T] => head.toString + ", " + tail.tailString
    case Nil => head.toString + "]"
  }

  @tailrec
  final def get(index: Int): T = {

    if(index == 0) head

    else {
      if(!tail.isEmpty) tail.get(index - 1)
      else throw new IndexOutOfBoundsException
    }
  }

  override def toString: String = "[" + tailString
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {

  def isEmpty: Boolean = false

}

object Nil extends List[Nothing] {

  def isEmpty: Boolean = true

  def head: Nothing = throw new NoSuchElementException("Nil.head")

  def tail: Nothing = throw new NoSuchElementException("Nil.tail")

  override def toString: String = "Nil"
}

object List {
  def apply[T](): List[T] = Nil

  def apply[T](x: T): List[T] = new Cons(x, apply())

  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, apply(x2))
}

List()
List(1)
val x = List(1, 2)
val y = x prepend 0
y get 0
y get 1

val z = y prepend "Hello"
z get 0
z get 1