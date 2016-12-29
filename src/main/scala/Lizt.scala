import com.sun.org.apache.xpath.internal.functions.FuncFalse

object Lizt {

  def increment(l: List[Int]) = l.foldRight(Nil: List[Int]) { (a, acc) => a + 1 :: acc }

  def stringify(l: List[Double]) = l.foldRight(Nil: List[String]) { (a, acc) => a.toString :: acc }

  def map[A, B](l: List[A])(f: A => B): List[B] = l.foldRight(Nil: List[B]) { (a, acc) => f(a) :: acc }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = l.foldRight(Nil: List[A]) { (a, acc) =>
    if (f(a)) a :: acc
    else acc
  }

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l.foldRight(Nil: List[B]) { (a, acc) => f(a) ++ acc }

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l) { a => if (f(a)) List(a) else Nil }

  def sumInPlace(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
    case (Nil, Nil) => Nil
    case (Nil, _  ) => Nil
    case (_  , Nil) => Nil
    case (a :: as, b :: bs) => a + b :: sumInPlace(as, bs)
  }

  def zipWith[A,B,C](xs: List[A], ys: List[B])(f: (A, B) => C): List[C] = (xs, ys) match {
    case (Nil, _  ) => Nil
    case (_  , Nil) => Nil
    case (a :: as, b :: bs) => f(a, b) :: zipWith(as, bs)(f)
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => false
    case (x :: xs) if x == sub.head => (sup take sub.length) == sub || hasSubsequence(xs, sub)
    case (_ :: xs) => hasSubsequence(xs, sub)
  }
}

object TestLizt extends App {
  println(Lizt.increment(List(1,2,3))) // List(2,3,4)

  println(Lizt.stringify(List(4.0, 5.0, 6.0))) // List("4.0", "5.0", "6.0")

  println( Lizt.map(List(1,2,3))(_ * 2) ) // List(2, 4, 6)

  println( Lizt.filter(List("Hi", "Oranges", "Charger", "Pen"))(_.length > 3) ) // List(Oranges, Charger)

  println( Lizt.flatMap(List("Hello", "World"))(_.toCharArray.toList)) // List(H,e,l,l,o,W,o,r,l,d)

  println( Lizt.sumInPlace(List(1,2,3), List(4,5,6))) // List(5, 7, 9)

  println( Lizt.zipWith(List(1,2,3), List(4,5,6))(_ + _) ) // List(5, 7, 9)

  println( Lizt.hasSubsequence(List(1,2,3), List(1,2))) // True
  println( Lizt.hasSubsequence(List(1,2,3), List(4))) // False
}
