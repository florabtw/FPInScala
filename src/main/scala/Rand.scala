import collection.immutable.List
import scala.annotation.tailrec

object Rand {
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def double: Rand[Double] = map(_.nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](sa: Rand[A], sb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = sa(rng)
    val (b, rng3) = sb(rng2)

    (f(a, b), rng3)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    @tailrec
    def go(rng: RNG, fs: List[Rand[A]], res: List[A]): (List[A], RNG) = {
      if (fs.isEmpty) (res, rng)
      else {
        val (a, rng2) = fs.head(rng)
        go(rng, fs.tail, a :: res)
      }
    }

    go(rng, fs, Nil)
  }

  def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(_.nextInt))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(_.nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0) unit(mod)
    else nonNegativeLessThan(n)
  }

  def mapViaFlatmap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s) { a => unit(f(a)) }

  def map2ViaFlatmap[A, B, C](sa: Rand[A], sb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(sa) { a =>
    map(sb) { b => f(a, b) }
  }

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)
}
