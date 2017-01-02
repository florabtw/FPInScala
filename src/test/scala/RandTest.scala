import org.scalatest.{Matchers, WordSpec}

class RandTest extends WordSpec with Matchers {
  "Rand" must {
    "return double" in {
      val rng = RNG(42)

      val (double: Double, _) = Rand.double(rng)

      println(double)
    }

    "generate ints" in {
      val rng = RNG(42)

      val (ints, _) = Rand.ints(10)(rng)

      ints.length shouldBe 10
      println(ints)
    }
  }
}
