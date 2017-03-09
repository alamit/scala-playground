val xs = Array(1, 2, 3, 4)
xs map (_ * 2)

val s = "Hello World!"
s filter (_.isUpper)

val r1 = 1 until 5
val r2 = 1 to 5

s exists (_.isUpper)
s forall (_.isUpper)

val pairs = 1 to 3
val pairs2 = List(1, 2, 3)
val zip1 = pairs zip s
val zip2 = pairs2 zip s

zip2.unzip

s flatMap (List('.', _))

xs.sum

List(('a', 1)).foldLeft(Map('b' -> 1))()
