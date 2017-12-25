package com.github.bhop.cats.semigroupal

import org.scalatest.{Matchers, WordSpec}

class ProductOfMonadsSpec extends WordSpec with Matchers {

  import ProductOfMonads._

  "A Product of Monads" should {

    "combine two monads" in {
      import cats.syntax.option._
      import cats.instances.option._
      product(5.some, 4.some) should be((5, 4).some)
      product(5.some, none[Int]) should be(none[Int])
    }
  }
}
