package parprog

/**
  * Created by adrienlamit on 26/02/17.
  */
package object week4 {

  implicit class ConcOps[T](val self: Conc[T]) extends AnyVal {
    def foreach[U](f: T => U) = Conc.traverse(self, f)
    def <>(that: Conc[T]) = Conc.concatTop(self.normalized, that.normalized)
  }

}
