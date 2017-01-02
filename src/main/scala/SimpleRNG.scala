import scala.annotation.tailrec

case class RNG(seed: Long) {
  private def nextSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL

  def nextInt: (Int, RNG) = {
    val newSeed = nextSeed
    val n = (newSeed >>> 16).toInt
    (n, RNG(newSeed))
  }

  def nonNegativeInt: (Int, RNG) = {
    val (newInt, newRNG) = nextInt
    if (newInt == Int.MinValue) (0, newRNG)
    else (Math.abs(newInt), newRNG)
  }

  def double: (Double, RNG) = {
    val (newInt, newRNG) = nonNegativeInt
    val newDouble = newInt / (Int.MaxValue.toDouble + 1)
    (newDouble, newRNG)
  }

  def intDouble: ((Int, Double), RNG) = {
    val (newInt, rng1) = nextInt
    val (newDouble, rng2) = rng1.double
    ((newInt, newDouble), rng2)
  }

  def doubleInt: ((Double, Int), RNG) = {
    val (newDouble, rng1) = double
    val (newInt, rng2) = rng1.nextInt
    ((newDouble, newInt), rng2)
  }

  def double3: ((Double, Double, Double), RNG) = {
    val (double1, rng1) = double
    val (double2, rng2) = double
    val (double3, rng3) = double
    ((double1, double2, double3), rng3)
  }

  def ints(count: Int): (List[Int], RNG) = {
    @tailrec
    def go(rng: RNG, ns: List[Int], count: Int): (List[Int], RNG) = {
      if (count <= 0) (ns, rng)
      else {
        val (newInt, newRNG) = rng.nextInt
        go(newRNG, newInt :: ns, count - 1)
      }
    }

    go(this, Nil, count)
  }
}
