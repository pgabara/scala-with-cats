package com.github.bhop.cats.monad

import org.scalatest.{Matchers, WordSpec}

class SumOfSquaresWithMonadsSpec extends WordSpec with Matchers {

  "A Sum of squares calculate" should {

    "calculate it for Id monads" in {
      import cats.Id
      import Monad.Instances._
      SumOfSquaresWithMonads.calculate(1: Id[Int], 2: Id[Int]) should be(5: Id[Int])
    }
  }
}
