
var uidCount = 0L
val x = new AnyRef {}
def getUniqueId(): Long = x synchronized {
  uidCount += 1
  uidCount
}

class Account(private var amount: Int = 0) {

  val uid = getUniqueId()

  private def lockAndTransfer(target: Account, n: Int) = this.synchronized {
    target.synchronized {
      this.amount -= n
      target.amount += n
    }
  }

  def transfer(target: Account, amount: Int) =
    if (this.uid < target.uid) {
      this.lockAndTransfer(target, amount)
    } else {
      target.lockAndTransfer(this, -amount)
    }
}

def startThread(a: Account, b: Account, n: Int) = {
  val t = new Thread {
    override def run(): Unit = {
      for (i <- 0 until n) {
        a.transfer(b, 1)
      }
    }
  }
  t.start()
  t
}

val a1 = new Account(500000)
val a2 = new Account(700000)

val t = startThread(a1, a2, 150000)
val s = startThread(a2, a1, 150000)

t.join()
s.join()