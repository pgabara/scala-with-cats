package com.github.bhop.cats.monad

import org.scalatest.{Matchers, WordSpec}

class WriterSpec extends WordSpec with Matchers {

  "A Writer" should {

    import cats.data.Writer

    "create an instance with just a value" in {
      import cats.syntax.applicative._
      import cats.instances.vector._
      type Logged[A] = cats.data.Writer[Vector[String], A]
      123.pure[Logged] should be (Writer(Vector.empty[String], 123))
    }

    "create an instance without a value" in {
      import cats.syntax.writer._
      Vector("message 1", "message 2").tell should be(Writer(Vector("message 1", "message 2"), ()))
    }

    "create an instance with both value and log" in {
      import cats.syntax.writer._
      123.writer(Vector("message 1")) should be(Writer(Vector("message 1"), 123))
    }

    "get value" in {
      Writer(Vector("message 1"), 123).value should be(123)
    }

    "get log" in {
      Writer(Vector("message 1", "message 2"), 123).written should be(Vector("message 1", "message 2"))
    }

    "get both value and log" in {
      Writer(Vector("message 1"), 123).run should be (Vector("message 1") -> 123)
    }

    "compose and transform writers" in {
      import cats.syntax.writer._
      import cats.syntax.applicative._
      import cats.instances.vector._

      type Logged[A] = cats.data.Writer[Vector[String], A]

      val program =
        for {
          x <- 10.pure[Logged]
          _ <- Vector("message 1", "message 2").tell
          y <- 32.writer(Vector("message 3"))
        } yield x + y

      val expected = 42.writer(Vector("message 1", "message 2", "message 3"))
      program.run should be(expected.run)
    }

    "transform log" in {
      import cats.syntax.writer._
      5.writer(Vector("a", "b", "c")).mapWritten(_.map(_.toUpperCase)).written should be(Vector("A", "B", "C"))
    }

    "transform both value and log" in {
      import cats.syntax.writer._
      5.writer(Vector("a")).bimap(_.map(_.toUpperCase), _.toString).run should be(Vector("A") -> "5")
    }

    "reset log" in {
      import cats.syntax.writer._
      import cats.instances.vector._
      "writer".writer(Vector("message 1", "message 2")).reset.written should be(Vector.empty[String])
    }

    "swap value with log" in {
      import cats.syntax.writer._
      5.writer("writer").swap should be("writer".writer(5))
    }
  }
}
