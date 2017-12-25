package com.github.bhop.cats.semigroupal

import com.github.bhop.cats.Cat
import org.scalatest.{Matchers, WordSpec}

class SemigroupalSpec extends WordSpec with Matchers {

  "A Semigroupal" should {

    "join/combine two contexts e.g. Options" in {
      import cats.Semigroupal
      import cats.syntax.option._
      import cats.instances.option._
      Semigroupal[Option].product("scala".some, "cats".some) should be(("scala", "cats").some)
      Semigroupal[Option].product("scala".some, none[String]) should be(none[String])

      // or using syntax...
      import cats.syntax.apply._
      ("scala".some, "cats".some).tupled should be(("scala", "cats").some)
    }

    "join/combine more than two contexts" in {
      import cats.Semigroupal
      import cats.syntax.option._
      import cats.instances.option._
      Semigroupal.tuple3(1.some, 2.some, 3.some) should be((1, 2, 3).some)
      Semigroupal.tuple3(1.some, none[Int], 3.some) should be(none[Int])

      // or using syntax...
      import cats.syntax.apply._
      (1.some, 2.some, 3.some).tupled should be((1, 2, 3).some)
    }

    "apply a user function to the values inside contexts" in {
      import cats.Semigroupal
      import cats.syntax.option._
      import cats.instances.option._
      Semigroupal.map3(1.some, 2.some, 3.some)(_ + _ + _ ) should be(6.some)
      Semigroupal.map3(1.some, none[Int], 3.some)(_ + _ + _ ) should be(none[Int])

      // or using syntax...
      import cats.syntax.apply._
      (1.some, 2.some, 3.some).mapN(_ + _ + _) should be(6.some)
      ("Garfield".some, 5.some, "black".some).mapN(Cat.apply) should be(Cat("Garfield", 5, "black").some)
    }
  }
}
