
sealed trait Tree[+A] {
  def size: Int = this match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + left.size + right.size
  }

  def maximum[B >: A](implicit num: Numeric[B]): B = this match {
    case Leaf(a) => a
    case Branch(left, right) => num.max(left.maximum(num), right.maximum(num))
  }

  def depth: Int = this match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + (left.depth max right.depth)
  }

  def map[B](f: A => B): Tree[B] = this match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(left.map(f), right.map(f))
  }

  def fold[B](f: A => B)(g: (B, B) => B): B = this match {
    case Leaf(a) => f(a)
    case Branch(left, right) => g(left.fold(f)(g), right.fold(f)(g))
  }

  def mapWithFold[B](f: A => B): Tree[B] = this.fold[Tree[B]]((a) => Leaf(f(a)))((b1, b2) => Branch(b1, b2))
  def depthWithFold: Int = this.fold[Int](_ => 1)((b1, b2) => 1 + (b1 max b2))
  def sizeWithFold: Int = this.fold[Int](_ => 1)((b1, b2) => 1 + b1 + b2)
}

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeTest extends App {
  val tree: Tree[Int] = Branch(Leaf(5), Branch(Leaf(6), Leaf(8)))

  println(tree.size) // 5

  println(tree.maximum) // 8

  println(tree.depth) // 3

  println(tree.map(_ * 2)) // Branch(Leaf(10), Branch(Leaf(12), Leaf(16))

  println(tree.mapWithFold(_ * 2)) // Branch(Leaf(10), Branch(Leaf(12), Leaf(16))
  println(tree.depthWithFold) // 3
  println(tree.sizeWithFold) // 5
}
