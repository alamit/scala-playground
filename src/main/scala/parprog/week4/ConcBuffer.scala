package parprog.week4

import parprog.week4.Conc.Chunk
import org.scalameter._

import scala.reflect.ClassTag

/**
  * Created by adrienlamit on 26/02/17.
  */
class ConcBuffer[T: ClassTag](val k: Int, private var conc: Conc[T]) {
  private var chunk: Array[T] = new Array(k)
  private var chunkSize: Int = 0

  require(k > 0)

  def this() = this(128, Conc.Empty)

  final def +=(elem: T): this.type = {
    if (chunkSize >= k) expand()

    chunk(chunkSize) = elem
    chunkSize += 1
    this

  }

  final def combine(that: ConcBuffer[T]): ConcBuffer[T] = {
    val combinedConc = this.result <> that.result
    new ConcBuffer(k, combinedConc)
  }

  private def result: Conc[T] = {
    conc = Conc.appendTop(conc, new Chunk(chunk, chunkSize, k))
    conc
  }

  private def expand() = {
    conc = Conc.appendTop(conc, new Chunk(chunk, chunkSize, k))
    chunk = new Array(k)
    chunkSize = 0
  }

  private def clear() {
    conc = Conc.Empty
    chunk = new Array(k)
    chunkSize = 0
  }
}

object ConcBuffer {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 60,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]) {
    val size = 1000000

    def run(p: Int) {
      val taskSupport = new collection.parallel.ForkJoinTaskSupport(
        new scala.concurrent.forkjoin.ForkJoinPool(p))
      val strings = (0 until size).map(_.toString)
      val time = standardConfig measure {
        val parallelized = strings.par
        parallelized.tasksupport = taskSupport
        parallelized.aggregate(new ConcBuffer[String])(_ += _, _ combine _).result
      }
      println(s"p = $p, time = $time ms")
    }

    run(1)
    run(2)
    run(4)
    run(8)
  }

}
