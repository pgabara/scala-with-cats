package com.github.bhop.cats.monad

import org.scalatest.{Matchers, WordSpec}

class FactorialWithWriterMonadSpec extends WordSpec with Matchers {

  import FactorialWithWriterMonad._

  "A Factorial calculator" should {

    "calculate factorial" in {
      factorial(3).value should be(6)
    }

    "return log" in {
      factorial(3).written should be(Vector("fact 0 => 1", "fact 1 => 1", "fact 2 => 2", "fact 3 => 6"))
    }
  }
}
