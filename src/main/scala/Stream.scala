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

  def mapViaUnfold[B](f: A => B) = Stream.unfold(this) {
    case Empty => None
    case Cons(h, t) => Some(f(h()), t())
  }

  def takeViaUnfold(n: Int): Stream[A] = Stream.unfold((n, this)) {
    case (0, _) => None
    case (_, Empty) => None
    case (nn, Cons(h, t)) => Some(h(), (nn - 1, t()))
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = Stream.unfold(this) {
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zipWith[B, C](f: (A, B) => C)(bs: Stream[B]): Stream[C] = Stream.unfold((this, bs)) {
    case (Cons(ah, at), Cons(bh, bt)) => Some(f(ah(), bh()), (at(), bt()))
    case _ => None
  }

  def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] = Stream.unfold((this, bs)) {
    case (Empty, Empty) => None
    case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), Empty))
    case (Empty, Cons(h, t)) => Some((None, Some(h())), (Empty, t()))
    case (Cons(ah, at), Cons(bh, bt)) => Some((Some(ah()), Some(bh())), (at(), bt()))
  }

  def startsWith[B >: A](bs: Stream[B]): Boolean = this.zipAll(bs).forAll {
    case (Some(b1), Some(b2)) => b1 == b2
    case (_, None) => true
    case (None, _) => false
  }

  def tails: Stream[Stream[A]] = Stream.unfold(this) {
    case Empty => None
    case stream @ Cons(h, t) => Some(stream, t())
  } append Stream()

  def hasSubsequence[B >: A](bs: Stream[B]): Boolean = tails exists (_ startsWith bs)

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = this match {
    case Cons(h, t) =>
      val scanRight @ (Cons(hh, _)) = t().scanRight(z)(f)
      Stream(f(h(), hh())) concat scanRight
    case _ => Stream(z)
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

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def fibsHelp(i: Int): Int = i match {
      case 0 => 1
      case 1 => 1
      case _ => fibsHelp(i - 2) + fibsHelp(i - 1)
    }

    Stream.from(0).map(index => fibsHelp(index))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((v, s)) => Stream.cons(v, unfold(s)(f))
    case None => Stream.empty
  }

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  def fromViaUnfold(n: Int): Stream[Int] = unfold(0)(s => Some(s, s + 1))

  def fibsViaUnfold: Stream[Int] = unfold((1, 1)) { case (l, r) => Some(l, (r, l + r)) }
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
  println(Stream(1, 2, 3, 4, 3, 2, 1).takeWhileViaFoldRight(_ < 3).toList)

  println("Testing headOption...")
  println(Stream(1, 2, 3, 4).headOption)
  println(Stream().headOption)

  println("Testing map...")
  println(Stream(1, 2, 3, 4).map(_ + 2).toList)

  println("Testing filter...")
  println(Stream(4, 2, 1, 3, 5).filter(_ > 2).toList)

  println("Testing append...")
  println(Stream(1, 2, 3, 4).append(5.0).toList)

  println("Testing flatMap...")
  println(Stream(1, 2, 3, 4).flatMap((n) => Stream(1 to n: _*)).toList)

  println("Testing constant...")
  println(Stream.constant(1).takeWhile(_ == 1).headOption)
  println(Stream.constant("apples").take(3).toList)

  println("Testing from...")
  println(Stream.from(11).take(10).toList)

  println("Testing fibs...")
  println(Stream.fibs.take(10).toList)

  println("Testing constantViaUnfold...")
  println(Stream.constantViaUnfold(1).takeWhile(_ == 1).headOption)
  println(Stream.constantViaUnfold("apples").take(3).toList)

  println("Testing fromViaUnfold...")
  println(Stream.fromViaUnfold(11).take(10).toList)

  println("Testing fibsViaUnfold...")
  println(Stream.fibsViaUnfold.take(30).toList)

  println("Testing mapViaUnfold...")
  println(Stream(1, 2, 3, 4).mapViaUnfold(_ + 2).toList)

  println("Testing takeViaUnfold...")
  println(Stream(1, 2, 3, 4).takeViaUnfold(2).toList)
  println(Stream(1, 2, 3, 4).takeViaUnfold(0).toList)
  println(Stream(1, 2, 3, 4).takeViaUnfold(4).toList)
  println(Stream(1, 2, 3, 4).takeViaUnfold(10).toList)

  println("Testing takeWhileViaUnfold...")
  println(Stream(1, 2, 3, 4).takeWhileViaUnfold(_ < 3).toList)
  println(Stream(1, 2, 3, 4).takeWhileViaUnfold(_ > 5).toList)
  println(Stream(1, 2, 3, 4, 3, 2, 1).takeWhileViaUnfold(_ < 3).toList)

  println("Testing zipWith...")
  println(Stream(1, 2, 3, 4).zipWith[Int, Int](_ + _)(Stream(4, 3, 2, 1)).toList)
  println(Stream(8, 9).zipWith[Int, Int](_ - _)(Stream(4, 5, 6)).toList)
  println(Stream(8, 9, 10).zipWith[Int, Int](_ - _)(Stream(4, 5)).toList)

  println("Testing zipAll...")
  println(Stream(1, 2, 3).zipAll(Stream(4, 5, 6)).toList)
  println(Stream(1, 2).zipAll(Stream(4, 5, 6)).toList)
  println(Stream(1, 2, 3).zipAll(Stream(4, 5)).toList)

  println("Testing startsWith...")
  println(Stream(1, 2, 3) startsWith Stream(1, 2))
  println(Stream(1, 2, 3) startsWith Stream(1, 2, 3, 4))
  println(Stream(1, 2, 3) startsWith Stream(4))
  println(Stream() startsWith Stream(3))

  println("Testing tails...")
  println(Stream(1, 2, 3, 4, 5).tails.toList.map(_.toList))

  println("Testing scanRight...")
  println(Stream(1, 2, 3).scanRight(0)(_ + _).toList)
}
