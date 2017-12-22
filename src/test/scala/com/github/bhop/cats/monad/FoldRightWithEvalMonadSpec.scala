package com.github.bhop.cats.monad

import org.scalatest.{Matchers, WordSpec}

class FoldRightWithEvalMonadSpec extends WordSpec with Matchers {

  import FoldRightWithEvalMonad._

  "An Eval fold right" should {

    "fold non empty list" in {
      foldRight[Int, Int](List(1, 2, 3, 4, 5), 0)(_ + _).value should be(15)
    }

    "fold list with only one element" in {
      foldRight[Int, Int](List(5), 0)(_ + _).value should be(5)
    }

    "fold empty list" in {
      foldRight[Int, Int](List.empty, 0)(_ + _).value should be(0)
    }
  }
}
