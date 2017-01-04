import scala.annotation.tailrec

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State(s => {
    val (a, newS) = run(s)
    (f(a), newS)
  })

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State(s => {
    val (a, s2) = run(s)
    val (b, s3) = sb.run(s2)

    (f(a, b), s3)
  })

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s2) = run(s)
    f(a).run(s2)
  })
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = State(s => {
    @tailrec
    def go(s: S, fs: List[State[S, A]], res: List[A]): (List[A], S) = {
      if (fs.isEmpty) (res, s)
      else {
        val (a, s2) = fs.head.run(s)
        go(s2, fs.tail, a :: res)
      }
    }

    go(s, fs, Nil)
  })

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

object StatePrint extends App {
  println(State.unit(5).run("okay"))
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
  @tailrec
  private def go(inputs: List[Input], machine: Machine): ((Int, Int), Machine) = inputs match {
    case Coin :: tail if machine.candies > 0 && machine.locked => go(tail, Machine(false, machine.candies, machine.coins + 1))
    case Turn :: tail if !machine.locked => go(tail, Machine(true, machine.candies - 1, machine.coins))
    case _ :: tail => go(tail, machine)
    case Nil => ((machine.candies, machine.coins), machine)
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State[Machine, (Int, Int)](m => go(inputs, m))
}