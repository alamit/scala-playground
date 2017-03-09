import scala.util.Random

trait Generator [+T] {
  self =>

  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    override def generate: S = f(self.generate)
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    override def generate: S = f(self.generate).generate
  }
}

val integers = new Generator[Int] {
  override def generate: Int = Random.nextInt()
}

val booleans = integers map (_ > 0)

trait Tree

case class Inner(left: Tree, right: Tree) extends Tree
case class Leaf(x: Int) extends Tree

def leaves: Generator[Leaf] = for {
  x <- integers
} yield Leaf(x)

def inners: Generator[Inner] = for {
  l <- trees
  r <- trees
} yield Inner(l, r)

def trees: Generator[Tree] = for {
  isLeaf <- booleans
  tree <- if (isLeaf) leaves else inners
} yield tree

trees.generate