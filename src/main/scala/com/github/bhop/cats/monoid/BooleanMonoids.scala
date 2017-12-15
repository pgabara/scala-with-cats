package com.github.bhop.cats.monoid

object BooleanMonoids {

  import cats.Monoid

  implicit val booleanAndMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      override def combine(x: Boolean, y: Boolean): Boolean = x && y
      override def empty: Boolean = true
    }

  implicit val booleanOrMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      override def combine(x: Boolean, y: Boolean): Boolean = x || y
      override def empty: Boolean = false
    }

  implicit val booleanEitherMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      override def combine(x: Boolean, y: Boolean): Boolean = (x && !y) || (!x && y)
      override def empty: Boolean = false
    }

  implicit val booleanXnorMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      override def combine(x: Boolean, y: Boolean): Boolean = (!x || y) && (x || !y)
      override def empty: Boolean = true
    }
}
