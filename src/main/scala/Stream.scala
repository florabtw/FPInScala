sealed trait Stream[+A] {
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = {
    if (n == 0) Empty
    else this match {
      case Empty => Empty
      case Cons(h, t) => Stream.cons(h(), t().take(n - 1))
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}

object StreamTest extends App {
  println(Stream(1,2,3))
  println(Stream(1,2,3).toList)

  println("Testing take...")
  println(Stream(1,2,3,4).take(2).toList)
  println(Stream(1,2,3,4).take(0).toList)
  println(Stream(1,2,3,4).take(4).toList)
  println(Stream(1,2,3,4).take(10).toList)

  println("Testing drop...")
  println(Stream(1,2,3,4).drop(2).toList)
  println(Stream(1,2,3,4).drop(0).toList)
  println(Stream(1,2,3,4).drop(4).toList)
  println(Stream(1,2,3,4).drop(10).toList)

  println("Testing takeWhile...")
  println(Stream(1,2,3,4).takeWhile(_ < 3).toList)
  println(Stream(1,2,3,4).takeWhile(_ > 5).toList)
}
