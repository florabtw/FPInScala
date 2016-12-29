import scala.annotation.tailrec

object Leest {

  def tail[A](list: List[A]) = drop(list, 1)

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) return l

    l match {
      case Nil => Nil
      case _ :: xs => drop(xs, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case a :: as if f(a) => dropWhile(as, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case _ :: Nil => Nil
    case x :: xs => x :: init(xs)
  }

  def setHead[A](list: List[A], head: A) = list match {
    case Nil => Nil
    case _ :: xs => head :: xs
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case x :: xs => f(x, foldRight(xs, z)(f))
  }

  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case x :: xs => foldLeft(xs, f(z, x))(f)
  }

  def length[A](as: List[A]): Int = foldLeft(as, 0)((acc, a) => acc + 1)

  def sum(l: List[Int]) = foldLeft(l, 0)(_ + _)

  def product(l: List[Double]) = foldLeft(l, 1.0)(_ * _)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])(_.+:(_))

  def foldLeftAgain[A,B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(as.reverse, z)((a, b) => f(b, a))

  def foldRightAgain[A,B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(as.reverse, z)((b, a) => f(a, b))

  def append[A](xs: List[A], ys: List[A]): List[A] = foldRight(xs, ys)((x, acc) => x :: acc)

  def concat[A](ls: List[List[A]]): List[A] = foldRight(ls, Nil: List[A])((acc, a) => append(acc, a))
}

object TestLeest extends App {

  println(Leest.tail(List(1, 2, 3))) // List(2,3)

  println(Leest.setHead(List(1,2,3), 4)) // List(4,2,3)

  println(Leest.drop(List(1,2,3,4), 3)) // List(4)

  println(Leest.dropWhile(List(1,2,3,4), (a: Int) => a < 3)) // List(3, 4)

  println(Leest.init(List(1,2,3,4))) // List(1,2,3)

  println(Leest.length(List(1,2,3))) // 3

  println(Leest.foldLeft(List(1,2,3), 1)(_ * _)) // 6

  println(Leest.sum(List(1,2,3))) // 6

  println(Leest.product(List(3,4,5))) // 60.0

  println(Leest.reverse(List(6,7,8))) // List(8,7,6)

  println(Leest.foldLeftAgain(List(3, 4, 5), 0)(_ + _)) // 12
  println(Leest.foldLeftAgain(List(2, 2, 2), 1)(_ * _)) // 8
  println(Leest.foldLeftAgain(List(1,2,3), Nil: List[Int])((acc, a) => a :: acc)) // List(3, 2, 1)

  println(Leest.foldRightAgain(List(1,2,3), Nil: List[Int])(_ :: _)) // List(1,2,3)

  println(Leest.append(List(1,2,3), List(4,5,6))) // List(1,2,3,4,5,6)

  println(Leest.concat(List(List(4,5), List(1,2), List(8,9)))) // List(4,5,1,2,8,9)
}
