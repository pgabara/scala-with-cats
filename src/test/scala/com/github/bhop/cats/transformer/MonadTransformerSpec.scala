package com.github.bhop.cats.transformer

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{Matchers, WordSpec}

import scala.concurrent.Future

class MonadTransformerSpec extends WordSpec with Matchers with ScalaFutures {

  "A Monad Transformer" should {

    import cats.data._

    "create an instance" in {
      type ErrorOr[A] = Either[String, A]
      type ErrorOrOption[A]= OptionT[ErrorOr, A]

      import cats.syntax.applicative._
      import cats.syntax.option._
      import cats.syntax.either._
      import cats.instances.either._
      5.pure[ErrorOrOption] should be { OptionT(5.some.asRight) }
    }

    "compose two instances" in {
      type ErrorOr[A] = Either[String, A]
      type ErrorOrOption[A]= OptionT[ErrorOr, A]

      import cats.syntax.applicative._
      import cats.syntax.option._
      import cats.syntax.either._
      import cats.instances.either._

      val program = for {
        a <- 5.pure[ErrorOrOption]
        b <- 4.pure[ErrorOrOption]
      } yield a + b

      program.value should be { 9.some.asRight }
    }

    "stack more than two monads" in {
      type FutureEither[A] = EitherT[Future, String, A]
      type FutureEitherOption[A] = OptionT[FutureEither, A]

      import cats.syntax.option._
      import cats.syntax.either._
      import cats.syntax.applicative._
      import cats.instances.all._

      import scala.concurrent.ExecutionContext.Implicits.global

      val program = for {
        a <- 5.pure[FutureEitherOption]
        b <- 4.pure[FutureEitherOption]
      } yield a + b

      program.value.value.futureValue should be { 9.some.asRight }
    }
  }
}
