package com.github.bhop.cats.monad

object FactorialWithWriterMonad {

  import cats.data.Writer
  import cats.syntax.writer._
  import cats.syntax.applicative._
  import cats.instances.vector._

  type Logged[A] = Writer[Vector[String], A]

  def factorial(n: Int): Logged[Int] = {
    for {
      x <- if (n == 0) 1.pure[Logged] else factorial(n - 1).map(_ * n)
      _ <- Vector(s"fact $n => $x").tell
    } yield x
  }
}
