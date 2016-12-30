import org.scalatest.{Matchers, WordSpec}

class LeestTest extends WordSpec with Matchers {

  "Leest" must {
    "have a tail" in {
      Leest.tail(List(1, 2, 3)) shouldBe List(2, 3)
    }
  }
}
