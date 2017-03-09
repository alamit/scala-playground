

def sum(xs: Array[Int]): Int = {
  xs.par.fold(0)(_ + _)
}

def max(xs: Array[Int]): Int = {
  xs.par.fold(Int.MinValue)(_ max _)
}

def play(a: String, b: String): String = List(a, b).sorted match {
  case List("paper", "scissors") => "scissors"
  case List("paper", "rock") => "paper"
  case List("rock", "scissors") => "rock"
  case List(a, b) if a == b => a
  case List("", b) => b
}

Array("paper", "rock", "paper", "scissors").par.fold("")(play)

sum(Array(1, 2, 3))
max(Array(1, 2, 3))

def eval(str: String): Int = {

}