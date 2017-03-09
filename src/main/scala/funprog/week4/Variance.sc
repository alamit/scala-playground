class SuperA {
  override def toString: String = "A1"
}

class A extends SuperA {
  override def toString: String = "A2"
}

class SuperB {
  override def toString: String = "B2"
}

class B extends SuperB {
  override def toString: String = "B1"
}

def f(x: SuperA): B = new B

def g(x: A): SuperB = new SuperB

f(new A)
g(new A)

val a1ToB1: SuperA => B = f
val a2ToB2: A => SuperB = f

// For a function to be a subtype of another, we want the function to allow us,
// to use it with all the arguments we can pass to its supertype
// and we want its result to be usable as any result of its supertype

// Which means its argument type must be A subtype of the supertype function's args
// And the result type must be a supertype of the supertype function's result

// Example with IntSets :

type IntSet = Int => Boolean