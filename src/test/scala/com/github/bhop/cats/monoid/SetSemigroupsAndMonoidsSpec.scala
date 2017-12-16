package com.github.bhop.cats.monoid

import org.scalatest.{Matchers, WordSpec}

class SetSemigroupsAndMonoidsSpec extends WordSpec with Matchers {

  case class Case(monoid: cats.Monoid[Set[Int]], name: String)

  val monoidCases = Seq(
    Case(monoid = SetSemigroupsAndMonoids.setUnionMonoid(), name = "union"),
    Case(monoid = SetSemigroupsAndMonoids.setSymmetricDifferenceMonoid(), name = "symmetric difference")
  )

  val setA = Set(2, 4, 6, 8)
  val setB = Set(4, 6, 8, 10)
  val setC = Set(2, 6, 10, 14)

  "Set monoids and semigroups" when {

    import cats.Monoid
    import cats.syntax.semigroup._

    for(Case(monoid, name) <- monoidCases) {
      s"$name set monoid" should {

        implicit val m: Monoid[Set[Int]] = monoid

        "obey associative law" in {
          setA |+| (setB |+| setC) shouldBe (setA |+| setB)  |+| setC
        }

        "obey identity law" in {
          (setA |+| Monoid[Set[Int]].empty shouldBe setA) == (Monoid[Set[Int]].empty |+| setA shouldBe setA)
        }
      }
    }

    "intersect set semigroup" should {

      "obey associative law" in {
        implicit val m: cats.Semigroup[Set[Int]] = SetSemigroupsAndMonoids.setIntersectSemigroup()
        setA |+| (setB |+| setC) shouldBe (setA |+| setB) |+| setC
      }
    }
  }
}
