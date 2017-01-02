import org.scalatest.{Matchers, WordSpec}

class SimpleRNGTest extends WordSpec with Matchers {
  val secretToMinValue = -9223372035308644558L

  "secret" must {
    "work" in {
      val rng = RNG(secretToMinValue)
      rng.nextInt._1 shouldBe Int.MinValue
    }
  }

  "SimpleRNG" must {
    "generate nonnegative integers" in {
      val time = System.currentTimeMillis()
      val rng = RNG(time)
      rng.nonNegativeInt._1 shouldBe >= (0)
    }

    "handle nonnegative edge-case" in {
      val rng = RNG(secretToMinValue)
      rng.nonNegativeInt._1 shouldBe >= (0)
    }

    "return a random list of ints" in {
      val rng = RNG(System.currentTimeMillis())
      val (ns, _) = rng.ints(10)

      ns.length shouldBe 10
      println(ns)
    }
  }
}
