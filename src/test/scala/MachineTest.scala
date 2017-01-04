import org.scalatest.{Matchers, WordSpec}

class MachineTest extends WordSpec with Matchers {

  "Machine" must {
    "simulate machine" in {
      val machine = Machine(true, 10, 0)

      val res = Machine.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn)).run(machine)

      res._1 shouldBe (7, 3)
    }
  }
}
