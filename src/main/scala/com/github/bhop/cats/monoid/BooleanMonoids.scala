package com.github.bhop.cats.monoid

object BooleanMonoids {

  import cats.Monoid

  val booleanAndMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      override def combine(x: Boolean, y: Boolean): Boolean = x && y
      override def empty: Boolean = true
    }

  val booleanOrMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      override def combine(x: Boolean, y: Boolean): Boolean = x || y
      override def empty: Boolean = false
    }

  val booleanEitherMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      override def combine(x: Boolean, y: Boolean): Boolean = (x && !y) || (!x && y)
      override def empty: Boolean = false
    }

  val booleanXnorMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      override def combine(x: Boolean, y: Boolean): Boolean = (!x || y) && (x || !y)
      override def empty: Boolean = true
    }
}
