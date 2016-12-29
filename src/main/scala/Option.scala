sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMapCaseMatch[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(_) => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(a) => if (f(a)) this else None
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object OptionTest extends App {
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def mean(xs: Seq[Double]): Option[Double] = xs match {
    case Nil => None
    case _ => Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = mean(xs) flatMap { m => mean(xs.map(x => Math.pow(x - m, 2))) }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for(someA <- a; someB <- b) yield f(someA, someB)

  def sum(one: Int, two: Int): Int = one + two

  println(map2(Some(3), Some(4))(sum))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldLeft[Option[List[A]]](Some(List[A]())) { (acc, next) => acc.flatMap { xs => next.map { n => xs :+ n } } }
  }

  def seq[A](as: List[Option[A]]): Option[List[A]] = as match {
    case Nil => Some(Nil)
    case head :: Nil => head.map(List(_))
    case head :: tail => head.flatMap { a => seq(tail).map { a :: _ } }
  }

  println(sequence(List(Some(1), Some(2))))
  println(sequence(List(Some(3), None)))
  println(seq(List(Some(1), Some(2))))
  println(seq(List(Some(3), None)))

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match {
    case Nil => Some(Nil)
    case h :: t => f(h).flatMap(hh => traverse(t)(f).map(hh :: _))
  }

  println(traverse(List("5", "6"))(str => Try(str.toInt)))
  println(traverse(List("3", "boo"))(str => Try(str.toInt)))

  def seqWithTraverse[A](as: List[Option[A]]): Option[List[A]] = traverse[Option[A], A](as)(a => a)
}
