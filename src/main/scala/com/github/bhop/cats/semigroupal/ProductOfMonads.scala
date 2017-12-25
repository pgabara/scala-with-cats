package com.github.bhop.cats.semigroupal

object ProductOfMonads {

  import cats.Monad
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def product[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)
}
