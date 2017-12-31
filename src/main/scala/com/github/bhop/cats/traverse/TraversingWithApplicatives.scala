package com.github.bhop.cats.traverse

object TraversingWithApplicatives {

  import cats.Applicative
  import cats.syntax.apply._
  import cats.syntax.applicative._

  def listTraverse[F[_]: Applicative, A, B](l: List[A])(f: A => F[B]): F[List[B]] =
    l.foldLeft(List.empty[B].pure[F]) { (acc, item) =>
      (acc, f(item)).mapN(_ :+ _)
    }

  def listSequence[F[_]: Applicative, B](l: List[F[B]]): F[List[B]] =
    listTraverse(l)(identity)
}
