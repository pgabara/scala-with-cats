package com.github.bhop.cats.casestudies.validation.v2

import cats.Semigroup
import cats.data.Validated

import cats.syntax.apply._
import cats.syntax.semigroup._
import cats.syntax.validated._

sealed trait Predicate[E, A] {

  def and(that: Predicate[E, A]): Predicate[E, A] =
    Predicate.And(this, that)

  def or(that: Predicate[E, A]): Predicate[E, A] =
    Predicate.Or(this, that)

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
    this match {
      case Predicate.Pure(func) =>
        func(a)

      case Predicate.And(left, right) =>
        (left(a), right(a)).mapN((_, _) => a)

      case Predicate.Or(left, right) =>
        left(a) match {
          case Validated.Valid(_) =>Validated.Valid(a)
          case Validated.Invalid(e1) =>
            right(a) match {
              case Validated.Valid(_) => Validated.Valid(a)
              case Validated.Invalid(e2) => Validated.Invalid(e1 |+| e2)
            }
        }
    }

  def run(implicit s: Semigroup[E]): A => Either[E, A] =
    (a: A) => this(a).toEither
}

object Predicate {

  final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]

  final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]

  final case class Pure[E, A](func: A => Validated[E, A]) extends Predicate[E, A]

  def apply[E, A](f: A => Validated[E, A]): Predicate[E, A] = Pure(f)

  def lift[E, A](error: E, func: A => Boolean): Predicate[E, A] =
    Pure(a => if(func(a)) a.valid else error.invalid)
}
