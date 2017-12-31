package com.github.bhop.cats.traverse

import org.scalatest.{Matchers, WordSpec}

class TraversingWithApplicativesSpec extends WordSpec with Matchers {

  import TraversingWithApplicatives._

  "Traversing with Applicatives" should {

    "using listSequence on a list of 2 vector parameters" in {
      import cats.instances.vector._
      val result = Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4))
      listSequence(List(Vector(1, 2), Vector(3, 4))) should be(result)
    }

    "using listSequence on a list of 3 vector parameters" in {
      import cats.instances.vector._
      val result = Vector(List(1, 3, 5), List(1, 3, 6), List(1, 4, 5), List(1, 4, 6), List(2, 3, 5),
        List(2, 3, 6), List(2, 4, 5), List(2, 4, 6))
      listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6))) should be(result)
    }

    "using listTraverse with options" in {
      import cats.syntax.option._
      process(List(2, 4, 6)) should be(List(2, 4, 6).some)
      process(List(1, 2, 3)) should be(none[List[Int]])

      def process(l: List[Int]): Option[List[Int]] = {
        import cats.instances.option._
        listTraverse(l)(n => if (n % 2 == 0) n.some else none[Int])
      }
    }

    "using listTraverse with validated data" in {
      import cats.data.Validated

      process(List(2, 4, 6)).isValid should be(true)
      process(List(1, 2, 3)).isInvalid should be(true)

      type ErrorsOr[A] = Validated[List[String], A]

      def process(l: List[Int]): ErrorsOr[List[Int]] = {
        import cats.instances.list._
        listTraverse(l) { n =>
          if (n % 2 == 0) Validated.valid(n)
          else Validated.invalid(List(s"$n is not even"))
        }
      }
    }
  }
}
