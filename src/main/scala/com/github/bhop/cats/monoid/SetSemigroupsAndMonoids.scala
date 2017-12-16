package com.github.bhop.cats.monoid

object SetSemigroupsAndMonoids {

  import cats.Monoid
  import cats.Semigroup

  def setUnionMonoid[A](): Monoid[Set[A]] =
    new Monoid[Set[A]] {
      override def combine(x: Set[A], y: Set[A]): Set[A] = x union y
      override def empty: Set[A] = Set.empty
    }

  def setIntersectSemigroup[A](): Semigroup[Set[A]] =
    (x: Set[A], y: Set[A]) => x intersect y

  def setSymmetricDifferenceMonoid[A](): Monoid[Set[A]] =
    new Monoid[Set[A]] {
      override def combine(x: Set[A], y: Set[A]): Set[A] = (x diff y) union (y diff x)
      override def empty: Set[A] = Set.empty
    }
}