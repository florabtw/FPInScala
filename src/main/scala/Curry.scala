object Curry {
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))
}

object CurryTest extends App {
  def sum(a: Int, b: Int) = a + b

  val curried = Curry.curry(sum)

  println(curried(1)(2)) // 3

  val uncurried = Curry.uncurry(curried)

  println(uncurried(3, 4)) // 7

  val addOne = (a: Int) => 1 + a
  val addTwo = (a: Int) => 2 + a

  val addThree = Curry.compose(addOne, addTwo)

  println(addThree(5)) // 8
}

