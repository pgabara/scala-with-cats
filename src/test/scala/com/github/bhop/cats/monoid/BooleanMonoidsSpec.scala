package com.github.bhop.cats.monoid

import org.scalatest.WordSpec

class BooleanMonoidsSpec extends WordSpec {

  case class Case(monoid: cats.Monoid[Boolean], name: String)

  val cases = Seq(
    Case(monoid = BooleanMonoids.booleanAndMonoid, name = "And"),
    Case(monoid = BooleanMonoids.booleanOrMonoid, name = "Or"),
    Case(monoid = BooleanMonoids.booleanEitherMonoid, name = "Either"),
    Case(monoid = BooleanMonoids.booleanXnorMonoid, name = "Xnor")
  )

  "Boolean monoids" when {

    import cats.Monoid
    import cats.syntax.semigroup._

    for (Case(monoid, name) <- cases) {
      s"boolean $name monoid" should {

        implicit val m: Monoid[Boolean] = monoid

        "obey associative law" in {
          true |+| (true  |+| true) == (true |+| true)  |+| true
          true |+| (false |+| true) == (true |+| false) |+| true
        }

        "obey identity law" in {
          (true  |+| Monoid[Boolean].empty == true)  && (Monoid[Boolean].empty |+| true == true)
          (false |+| Monoid[Boolean].empty == false) && (Monoid[Boolean].empty |+| false == false)
        }
      }
    }

  }
}
