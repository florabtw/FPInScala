import annotation.tailrec

object Fib {
  def fibonacci(n: Int): Int = {
    @tailrec
    def go(n: Int, prev: Int, current: Int): Int = {
      if (n == 0) prev
      else go(n - 1, current, current + prev)
    }

    go(n, 0, 1)
  }

  def main(args: Array[String]) = {
    val answer = fibonacci(6)
    println(answer)
  }
}
