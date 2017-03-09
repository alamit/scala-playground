import org.scalameter._

val mem = config(
  Key.exec.benchRuns -> 20
) withMeasurer(new Measurer.MemoryFootprint) measure {
  for (i <- 0 until 1000000) yield i
}
println(s"Total memory: $mem")

val time = config(
  Key.exec.benchRuns -> 20,
  Key.verbose -> true
) withWarmer {
  new Warmer.Default
} withMeasurer {
  new Measurer.IgnoringGC
} measure {
  for (i <- 0 until 1000000) yield i
}
println(s"Total time: $time")