

val points = sc.textFile(...).map(parsePoint).persist()

var w = Vector.zeros(d)

for(i <- 1 to numIterations) {
  val gradient = points.map {
    p => (1 / (1 + exp(-p.y * w.dot(p.x))) - 1) * p.y * p.y
  }.reduce(_ + _)
  w -= alpha * gradient
}

