trait Expr {
  def show: String = this match {
    case Number(x) => x.toString
    case Sum(l, r) => l.show + " + " + r.show
    case Var(s) => s
    case Product(l, r) => par(l) + " * " + par(r)
  }

  private def par(e: Expr): String = e match {
    case Sum(_, _) => "(" + e.show + ")"
    case _ => e.show
  }

}

case class Number(x: Int) extends Expr
case class Sum(left: Expr, right: Expr) extends Expr
case class Product(left: Expr, right: Expr) extends Expr
case class Var(name: String) extends Expr

Sum(Number(1), Number(43)).show
Sum(Product(Number(1), Number(3)), Number(5)).show
Sum(Product(Number(2), Var("x")), Var("y")).show
Product(Sum(Number(2), Var("x")), Var("y")).show