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

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = foldRight(Stream[A]())((a, b) => if (p(a)) Stream.cons(a, b) else Empty)

  def headOption: Option[A] = foldRight[Option[A]](None)((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] = foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))

  def filter[B](p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((a, b) => {
    if (p(a)) Stream.cons(a, b)
    else b
  })

  def append[S >: A](a: S): Stream[S] = foldRight(Stream(a))((a, b) => Stream.cons(a, b))

  def concat[S >: A](as: Stream[S]): Stream[S] = foldRight(as)((a, b) => Stream.cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])((a, b) => f(a) concat b)
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
  println(Stream(1, 2, 3))
  println(Stream(1, 2, 3).toList)

  println("Testing take...")
  println(Stream(1, 2, 3, 4).take(2).toList)
  println(Stream(1, 2, 3, 4).take(0).toList)
  println(Stream(1, 2, 3, 4).take(4).toList)
  println(Stream(1, 2, 3, 4).take(10).toList)

  println("Testing drop...")
  println(Stream(1, 2, 3, 4).drop(2).toList)
  println(Stream(1, 2, 3, 4).drop(0).toList)
  println(Stream(1, 2, 3, 4).drop(4).toList)
  println(Stream(1, 2, 3, 4).drop(10).toList)

  println("Testing takeWhile...")
  println(Stream(1, 2, 3, 4).takeWhile(_ < 3).toList)
  println(Stream(1, 2, 3, 4).takeWhile(_ > 5).toList)

  println("Testing forAll...")
  println(Stream(1, 2, 3, 4).forAll(_ < 10))
  println(Stream(1, 2, 3, 4).forAll(_ > 2))

  println("Testing takeWhileViaFoldRight...")
  println(Stream(1, 2, 3, 4).takeWhileViaFoldRight(_ < 3).toList)
  println(Stream(1, 2, 3, 4).takeWhileViaFoldRight(_ > 5).toList)
  println(Stream(1,2,3,4,3,2,1).takeWhileViaFoldRight(_ < 3).toList)

  println("Testing headOption...")
  println(Stream(1,2,3,4).headOption)
  println(Stream().headOption)

  println("Testing map...")
  println(Stream(1,2,3,4).map(_ + 2).toList)

  println("Testing filter...")
  println(Stream(4, 2, 1, 3, 5).filter(_ > 2).toList)

  println("Testing append...")
  println(Stream(1, 2, 3, 4).append(5.0).toList)

  println("Testing flatMap...")
  println(Stream(1, 2, 3, 4).flatMap((n) => Stream(1 to n: _*)).toList)
}
