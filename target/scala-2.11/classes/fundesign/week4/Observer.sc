trait Publisher {

  private var subscribers: Set[Subscriber] = Set.empty

  def subscribe(subscriber: Subscriber): Unit =
    subscribers += subscriber

  def unsubscribe(subscriber: Subscriber): Unit =
    subscribers -= subscriber

  def publish(): Unit =
    subscribers foreach (_.handler(this))
}

trait Subscriber {
  def handler(publisher: Publisher): Unit
}

class BankAccount extends Publisher {

  private var balance: Int = 0

  def currentBalance: Int = balance

  def deposit(amount: Int): Unit =
    if(amount > 0) {
      balance += amount
      publish()
    }
    else ()

  def withdraw(amount: Int): Unit = {
    require(amount > 0 && amount <= balance)
    balance -= amount
    publish()
  }

}

class Consolidator(observed: List[BankAccount]) extends Subscriber {
  observed foreach (_.subscribe(this))

  private var total: Int = _
  compute()

  private def compute(): Unit = {
    total = observed map (_.currentBalance) sum
  }

  override def handler(publisher: Publisher): Unit = compute()

  def totalBalance = total
}

val a = new BankAccount
val b = new BankAccount
val c = new Consolidator(List(a, b))

c.totalBalance
a deposit 20
c.totalBalance
b deposit 30
c.totalBalance