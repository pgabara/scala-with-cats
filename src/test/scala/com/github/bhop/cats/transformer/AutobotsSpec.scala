package com.github.bhop.cats.transformer

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{Matchers, WordSpec}

class AutobotsSpec extends WordSpec with Matchers with ScalaFutures {

  import scala.concurrent.ExecutionContext.Implicits.global

  import Autobots._

  "Autobots" should {

    import cats.syntax.either._

    "get power level for defined autobot" in {
      getPowerLevel("Jazz").value.futureValue should be(6.asRight)
    }

    "return error if autobot is unreachable" in {
      getPowerLevel("Fake Bot").value.futureValue should be("Fake Bot unreachable".asLeft)
    }

    "return true if special move is available (power level of both autobots > 15)" in {
      canSpecialMove("Bumblebee", "Hot Rod").value.futureValue should be(true.asRight)
    }

    "return false if special move is unavailable (power level of both autobots <= 15)" in {
      canSpecialMove("Bumblebee", "Jazz").value.futureValue should be(false.asRight)
    }

    "return error if special move check cannot be applied (one of autobots is unreachable)" in {
      canSpecialMove("Jazz", "Fake Bot").value.futureValue should be("Fake Bot unreachable".asLeft)
    }

    "report that special attack is available" in {
      tacticalReport("Bumblebee", "Hot Rod").futureValue should be("Bumblebee and Hot Rod are ready to roll out!")
    }

    "report that special attack is unavailable" in {
      tacticalReport("Bumblebee", "Jazz").futureValue should be("Bumblebee and Jazz need a recharge.")
    }

    "report that one of autobots is unreachable" in {
      tacticalReport("Jazz", "Fake Bot").futureValue should be("Fake Bot unreachable")
    }
  }
}
