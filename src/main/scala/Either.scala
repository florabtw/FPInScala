sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case right => right
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for (self <- this; bb <- b) yield f(self, bb)

  def otherMap2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    this.flatMap { self => b.map { bb => f(self, bb) } }
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = Either.traverse[E, Either[E, A], A](es)(a => a)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(List())
    case h :: t => f(h).flatMap { hh => traverse(t)(f).map { hh :: _ } }
  }
}

object EitherTest extends App {
  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  println(Either.sequence(List(Right(1), Right(2))))
  println(Either.sequence(List(Right(3), Left("Uh oh!"))))

  println(Either.traverse(List("5", "6"))(str => Try(str.toInt)))
  println(Either.traverse(List("7", "woops"))(str => Try(str.toInt)))
}
