

abstract class Simulation {

  type Action = () => Unit

  case class Event(time: Int, action: Action)

  private type Agenda = List[Event]
  private var agenda: Agenda = Nil
  private var curTime = 0

  def currentTime: Int = curTime

  def afterDelay(delay: Int)(block: => Unit): Unit = {
    val item = Event(curTime + delay, () => block)
    agenda = insert(agenda, item)
  }

  private def insert(a: Agenda, e: Event): List[Event] = a match {
    case head :: tail if head.time <= e.time =>
      head :: insert(tail, e)
    case _ => e :: a
  }

  private def loop: Action = agenda match {
    case head :: tail =>
      agenda = tail
      curTime = head.time
      head.action()
      loop
    case Nil => () => ()
  }

  def run(): Unit = {
    afterDelay(0) {
      println("*** simulation started, time = " + currentTime + " ***")
    }
    loop
  }
}

abstract class Gates extends Simulation {

  def InverterDelay: Int
  def AndGateDelay: Int
  def OrGateDelay: Int

  class Wire {

    private var signal: Boolean = false
    private var actions: List[Action] = Nil

    def getSignal: Boolean = signal

    def setSignal(s: Boolean): Unit =
      if (s != signal) {
        signal = s
        actions foreach (_.apply)
      }


    def addAction(a: Action): Unit = {
      actions = a :: actions
      a.apply
    }
  }


  def inverter(input: Wire, output: Wire): Unit = {
    def invertAction(): Unit = {
      val inputSignal = input.getSignal
      afterDelay(InverterDelay) {
        output setSignal !inputSignal
      }
    }

    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire): Unit = {
    def orAction(): Unit = {
      val a1Signal = a1.getSignal
      val a2Signal = a2.getSignal
      afterDelay(AndGateDelay) {
        output setSignal (a1Signal & a2Signal)
      }
    }

    a1 addAction orAction
    a2 addAction orAction
  }

  def orGate(o1: Wire, o2: Wire, output: Wire): Unit = {
    def andAction(): Unit = {
      val o1Signal = o1.getSignal
      val o2Signal = o2.getSignal
      afterDelay(OrGateDelay) {
        output setSignal (o1Signal | o2Signal)
      }
    }

    o1 addAction andAction
    o2 addAction andAction
  }


  def probe(name: String, wire: Wire): Unit = {
    def probeAction(): Unit = {
      println(s"$name $currentTime value = ${wire.getSignal}")
    }
    wire addAction probeAction
  }
}

abstract class Circuit extends Gates {

  def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire): Unit = {

    val d, e = new Wire

    orGate(a, b, d)
    andGate(a, b, c)
    inverter(c, e)
    andGate(d, e, s)

  }

  def fullAdder(a: Wire, b: Wire, cin: Wire, sum: Wire, cout: Wire): Unit = {
    val s, c1, c2 = new Wire

    halfAdder(b, cin, s, c1)
    halfAdder(a, s, sum, c2)
    orGate(c1, c2, cout)
  }

}

trait Parameters {
  def InverterDelay = 2
  def AndGateDelay = 3
  def OrGateDelay = 5
}

object sim extends Circuit with Parameters
import sim._

val in1, in2, sum, carry = new Wire

halfAdder(in1, in2, sum, carry)
probe("sum", sum)
probe("carry", carry)

in1 setSignal(true)
run()

in2 setSignal(true)
run()

in1 setSignal(false)
run()