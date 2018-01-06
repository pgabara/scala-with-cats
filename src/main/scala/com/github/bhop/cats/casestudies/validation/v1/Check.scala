package com.github.bhop.cats.casestudies.validation.v1

import cats.Semigroup
import cats.data.Validated

sealed trait Check[E, A] {

  import Check._

  def and(that: Check[E, A]): And[E, A] = And(this, that)

  def apply(value: A)(implicit sg: Semigroup[E]): Validated[E, A] = {
    import cats.syntax.apply._
    import cats.syntax.validated._
    import cats.syntax.semigroup._
    this match {
      case Pure(func) => func(value)
      case And(left, right) => (left(value), right(value)).mapN((_, _) => value)
      case Or(left, right) =>
        (left(value), right(value)) match {
          case (Validated.Invalid(e1), Validated.Invalid(e2)) => (e1 |+| e2).invalid
          case _ => value.valid
        }
    }
  }
}

object Check {

  final case class And[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]

  final case class Or[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]

  final case class Pure[E, A](func: A => Validated[E, A]) extends Check[E, A]
}
