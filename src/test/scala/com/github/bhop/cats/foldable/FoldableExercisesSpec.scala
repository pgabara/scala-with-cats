package com.github.bhop.cats.foldable

import org.scalatest.{Matchers, WordSpec}

class FoldableExercisesSpec extends WordSpec with Matchers {

  "Reflecting on Folds" should {

    "use foldLeft with an empty list as the accumulator and :: as the binary operator (should reverse the list)" in {
      List(1, 2, 3).foldLeft[List[Int]](List.empty)((a, x) => x :: a) should be(List(3, 2, 1))
    }

    "use foldRight with an empty list as the accumulator and :: as the binary operator (should return the same list)" in {
      List(1, 2, 3).foldRight[List[Int]](List.empty)((x, a) => x :: a) should be(List(1, 2, 3))
    }
  }

  "Sca-fold-ing Other Methods" should {

    "list's map implemented via foldRight" in {
      map(List("1", "2", "3"))(_.toInt) should be(List(1, 2, 3))

      def map[A, B](la: List[A])(f: A => B): List[B] =
        la.foldRight(List.empty[B])((x, a) => f(x) :: a)
    }

    "list's flatMap implemented via foldRight" in {
      flatMap(List("1", "2", "3"))(x => List(x.toInt)) should be(List(1, 2, 3))

      def flatMap[A, B](la: List[A])(f: A => List[B]): List[B] =
        la.foldRight(List.empty[B]) { (x, a) => f(x) ++ a }
    }

    "list's filter implemented via foldRight" in {
      filter(List(1, 2, 3, 4))(_ > 2) should be(List(3, 4))

      def filter[A](la: List[A])(f: A => Boolean): List[A] =
        la.foldRight(List.empty[A]) { (x, a) => if (f(x)) x :: a else a }
    }

    "list's sum implemented via foldRight" in {
      import cats.Monoid
      import cats.instances.int._
      sum(List(1, 2, 3, 4)) should be(10)

      def sum[A](la: List[A])(implicit monoid: Monoid[A]): A =
        la.foldRight(monoid.empty)(monoid.combine)
    }
  }
}
