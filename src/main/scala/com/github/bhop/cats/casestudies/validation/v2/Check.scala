package com.github.bhop.cats.casestudies.validation.v2

import cats.data.Validated
import cats.Semigroup

sealed trait Check[E, A, B] {

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B]

  def map[C](func: B => C): Check[E, A, C] = Check.Map(this, func)

  def flatMap[C](func: B => Check[E, A, C]): Check[E, A, C] = Check.FlatMap(this, func)

  def andThen[C](that: Check[E, B, C]): Check[E, A, C] = Check.AndThen(this, that)
}

object Check {

  final case class Map[E, A, B, C](check: Check[E, A, B], func: B => C) extends Check[E, A, C] {
    override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(a).map(func)
  }

  final case class FlatMap[E, A, B, C](check: Check[E, A, B], func: B => Check[E, A, C]) extends Check[E, A, C] {
    override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(a).withEither(_.flatMap(b => func(b)(a).toEither))
  }

  final case class AndThen[E, A, B, C](left: Check[E, A, B], right: Check[E, B, C]) extends Check[E, A, C] {
    override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      left(a).andThen(right(_))
  }

  final case class Pure[E, A, B](func: A => Validated[E, B]) extends Check[E, A, B] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B] =
      func(a)
  }

  final case class PurePredicate[E, A](predicate: Predicate[E, A]) extends Check[E, A, A] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      predicate(a)
  }

  def apply[E, A](predicate: Predicate[E, A]): Check[E, A, A] =
    PurePredicate(predicate)

  def apply[E, A, B](func: A => Validated[E, B]): Check[E, A, B] =
    Pure(func)
}
