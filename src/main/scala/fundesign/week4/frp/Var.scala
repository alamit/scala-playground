package fundesign.week4.frp

/**
  * Created by adrienlamit on 11/02/17.
  */
object Var {
  def apply[T](expr: => T): Var[T] = new Var(expr)
}

class Var[T](expr: => T) extends Signal[T](expr) {
  override def update(expr: => T): Unit = super.update(expr)
}

