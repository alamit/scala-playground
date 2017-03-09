class REPEAT(command: => Unit) {

  def UNTIL(condition: => Boolean): Unit = {
    command
    if(condition) ()
    else UNTIL(condition)
  }

}

def REPEAT(command: => Unit): REPEAT = new REPEAT(command)
var x = 0

REPEAT {
  x = x + 1
  println(x)
} UNTIL (x == 10)

