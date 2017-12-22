package com.github.bhop.cats.monad

import org.scalatest.{Matchers, WordSpec}

class TryAsMonadSpec extends WordSpec with Matchers {

  import TryAsMonad._

  "A Tree monad" should {

    "flatMap" in {
      import cats.syntax.flatMap._
      branch(leaf(5), leaf(10)).flatMap(x => leaf(x.toString)) should be(branch(leaf("5"), leaf("10")))
    }

    "map" in {
      import cats.syntax.functor._
      branch(leaf(5), leaf(10)).map(_ + 2) should be(branch(leaf(7), leaf(12)))
    }

    "allow for for-comprehension" in {
      import cats.syntax.functor._
      import cats.syntax.flatMap._

      val value =
        for {
          a <- branch(leaf(100), leaf(200))
          b <- branch(leaf(a - 10), leaf(a + 10))
        } yield b

      value should be(
        branch(
          branch(leaf(90), leaf(110)), branch(leaf(190), leaf(210))
        )
      )
    }
  }
}
