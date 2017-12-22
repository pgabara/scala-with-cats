package com.github.bhop.cats.monad

object SumOfSquaresWithMonads {

  def calculate[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] = {
    import Monad.Syntax._
    for {
      x <- a
      y <- b
    } yield x*x + y*y
  }
}
