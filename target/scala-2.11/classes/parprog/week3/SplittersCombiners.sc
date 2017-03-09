import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.{Combiner, SeqSplitter, Splitter}
import scala.reflect.ClassTag

/*
trait Splitter[A] extends Iterator[A] {
  def split: Seq[Splitter[A]]
  def remaining: Int

  val threshold: Int = ???

  def fold(z: A)(f: (A, A) => A): A = {
    if(remaining < threshold) {
      foldLeft(z)(f)
    } else {
      val children: Seq[ForkJoinTask[A]] =
        split.map(child => task(fold(z)(f)))
      children.map(_.join()).foldLeft(z)(f)
    }
  }
}
*/

class ArrayCombiner[T <: AnyRef: ClassTag](val parallelism: Int) {

  private var numElems = 0
  private val buffers = new ArrayBuffer[ArrayBuffer[T]]()
  buffers += new ArrayBuffer[T]

  def +=(x: T): ArrayCombiner[T] = {
    buffers.last += x
    numElems += 1
    this
  }

  def combine(that: ArrayCombiner[T]): ArrayCombiner[T] = {
    buffers ++= that.buffers
    numElems += that.numElems
    this
  }
}

val a = new Combiner[T]
val b = new Splitter[] {}
val c = new SeqSplitter[] {}