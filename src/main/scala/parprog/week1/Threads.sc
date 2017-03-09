class HelloThread extends Thread {
  override def run(): Unit = {
    println("Hello world!")
  }
}

val t = new HelloThread

t.start()
t.join()

private var uidCount = 0L

def getUniqueId(): Long = {
  uidCount += 1
  uidCount
}

def startThread() = {
  val t = new Thread {
    override def run(): Unit = {
      val uids = for (i <- 0 until 10) yield getUniqueId()
      println(uids)
    }
  }
  t.start()
  t
}
