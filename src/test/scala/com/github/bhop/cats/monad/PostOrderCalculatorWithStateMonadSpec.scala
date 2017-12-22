package com.github.bhop.cats.monad

import org.scalatest.{Matchers, WordSpec}

class PostOrderCalculatorWithStateMonadSpec extends WordSpec with Matchers {

  import PostOrderCalculatorWithStateMonad._

  "A Post-Order Calculator" when {

    "throw an error if number of operand is not enough" in {
      intercept[IllegalStateException] {
        evalOne("+").run(List(5)).value
      }
    }

    "calculate the operation" in {
      evalOne("+").run(List(5, 10)).value should be(List(15) -> 15)
    }

    "return updated state" in {
      evalOne("5").run(List(1)).value should be(List(5, 1) -> 5)
    }

    "compose operations" in {
      val sum =
        for {
          _ <- evalOne("5")
          _ <- evalOne("5")
          r <- evalOne("*")
        } yield r
      sum.runA(List.empty).value should be(25)
    }

    "compose operations via evalAll" in {
      evalAll(List("5", "5", "*")).runA(List.empty).value should be(25)
    }

    "compose operations via eval" in {
      eval("5 5 * 1 + 1 - 5 /") should be(5)
    }
  }
}
