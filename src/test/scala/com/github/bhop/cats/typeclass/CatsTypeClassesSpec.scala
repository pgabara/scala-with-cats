package com.github.bhop.cats.typeclass

import com.github.bhop.cats.Cat
import org.scalatest.{Matchers, WordSpec}

class CatsTypeClassesSpec extends WordSpec with Matchers {

  "Cats type classes" when {

    "Show" should {

      "stringify cat instance" in {
        import cats.syntax.show._
        import Cat.Instances._
        Cat("Max", 6, "white").show should be("Max is a 6 year-old white cat.")
      }
    }

    "Eq" should {

      "compare two the same cats correctly" in {
        import cats.syntax.eq._
        import Cat.Instances._
        Cat("Max", 6, "white") =!= Cat("Max", 6, "white") should be(false)
      }

      "compare two different cats correctly" in {
        import cats.syntax.eq._
        import Cat.Instances._
        Cat("Max", 6, "white") =!= Cat("Max", 6, "black") should be(true)
      }

      "compare two the same cats wrapped in option" in {
        import cats.syntax.eq._
        import cats.syntax.option._
        import cats.instances.option._
        import Cat.Instances._
        Cat("Max", 6, "white").some =!= Cat("Max", 6, "white").some should be(false)
      }

      "compare cat wrapped in option with none instance" in {
        import cats.syntax.eq._
        import cats.syntax.option._
        import cats.instances.option._
        import Cat.Instances._
        Cat("Max", 6, "white").some =!= none[Cat] should be(true)
      }
    }
  }
}
