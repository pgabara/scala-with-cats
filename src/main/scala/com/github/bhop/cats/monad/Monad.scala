package com.github.bhop.cats.monad

trait Monad[F[_]] {

  def pure[A](a: A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(x => pure(f(x)))
}

object Monad {

  object Syntax {
    implicit class MonadOps[F[_], A](fa: F[A]) {
      def flatMap[B](f: A => F[B])(implicit m: Monad[F]): F[B] = m.flatMap(fa)(f)
      def map[B](f: A => B)(implicit m: Monad[F]): F[B] = m.map(fa)(f)
    }
  }

  object Instances {

    implicit val idMonad: Monad[cats.Id] =
      new Monad[cats.Id] {
        override def pure[A](a: A): cats.Id[A] = a
        override def flatMap[A, B](fa: cats.Id[A])(f: A => cats.Id[B]): cats.Id[B] = f(fa)
      }
  }
}
