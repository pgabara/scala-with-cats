package com.github.bhop.cats.validated

import org.scalatest.{Matchers, WordSpec}

import scala.util.Try

class ValidatedSpec extends WordSpec with Matchers {

  "A Validated" should {

    import cats.data.Validated

    "create valid instance" in {
      Validated.valid[List[String], String]("VALID") should be(Validated.Valid("VALID"))
      // or using syntax...
      import cats.syntax.validated._
      "VALID".valid[List[String]] should be(Validated.Valid("VALID"))
    }

    "create invalid instance" in {
      val errors = List("Error 1", "Error 2")
      Validated.invalid[List[String], String](errors) should be(Validated.Invalid(errors))
      // or using syntax...
      import cats.syntax.validated._
      errors.invalid[String] should be(Validated.Invalid(errors))
    }

    "create from exception" in {
      Validated.catchOnly[NumberFormatException]("foo".toInt) shouldBe a[Validated.Invalid[_]]
      Validated.catchOnly[NumberFormatException]("5".toInt) should be(Validated.Valid(5))
    }

    "create from Try" in {
      Validated.fromTry(Try("foo".toInt)) shouldBe a[Validated.Invalid[_]]
      Validated.fromTry(Try("5".toInt)) should be(Validated.valid(5))
    }

    "create from Either" in {
      import cats.syntax.either._
      Validated.fromEither("Error".asLeft) should be(Validated.Invalid("Error"))
      Validated.fromEither(5.asRight) should be(Validated.Valid(5))
    }

    "create from option" in {
      import cats.syntax.option._
      Validated.fromOption(none[Int], "Error") should be(Validated.Invalid("Error"))
      Validated.fromOption(5.some, "Error") should be(Validated.Valid(5))
    }

    "combine all errors" in {
      import cats.syntax.apply._
      import cats.syntax.validated._
      import cats.instances.vector._

      (
        Vector("Error 1").invalid[Int],
        Vector("Error 2").invalid[Int]
      ).tupled should be(Validated.Invalid(Vector("Error 1", "Error 2")))
    }

    "extract values (getOrElse)" in {
      import cats.syntax.validated._
      5.valid[String].getOrElse(0) should be(5)
      "Error".invalid[Int].getOrElse(0) should be(0)
    }

    "extract values (fold)" in {
      import cats.syntax.validated._
      5.valid[String].fold(x => s"Error: $x", _.toString) should be("5")
      "Invalid number!".invalid[Int].fold(x => s"Error: $x", _.toString) should be("Error: Invalid number!")
    }
  }
}
