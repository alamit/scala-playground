class BankAccount {
  private var balance = 0

  def deposit(amount: Int): Unit = if (amount > 0) balance = balance + amount

  def withdraw(amount: Int): Int = if (amount > 0 && amount <= balance) {
    balance = balance - amount
    balance
  } else throw new IllegalArgumentException("Insufficient founds")
}

val account = new BankAccount
account deposit 50
account withdraw 20
account withdraw 20
account withdraw 15