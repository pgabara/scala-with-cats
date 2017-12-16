package com.github.bhop.cats.typeclass

import com.github.bhop.cats.Cat
import org.scalatest.{Matchers, WordSpec}

class PrintableSpec extends WordSpec with Matchers {

  "A Printable" should {

    import Printable.Syntax._
    import Printable.Instances._

    "format string value" in {
      Printable.format("scala with cats") should be("scala with cats")
    }

    "format int value" in {
      45.format should be("45")
    }

    "format cat instance" in {
      import Cat.Instances._
      Cat("Max", 6, "white").format should be("Max is a 6 year-old white cat.")
    }

    "format box instance" in {
      case class Box[A](value: A)

      implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
        p.contramap(_.value)

      Box(value = true).format should be("yes")
    }
  }
}
