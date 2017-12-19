package com.github.bhop.cats.functor

import org.scalatest.{Matchers, WordSpec}

class ContravariantSpec extends WordSpec with Matchers {

  "A Contravariant functor" should {

    "create Show instance for symbols from string Show instance" in {
      import cats.Show
      import cats.syntax.show._
      import cats.syntax.contravariant._
      import cats.instances.string._
      implicit val symbolShow: Show[Symbol] = Show[String].contramap[Symbol](_.name)
      'cats.show should be("cats")
    }
  }
}
