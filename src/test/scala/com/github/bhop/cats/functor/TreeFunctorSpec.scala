package com.github.bhop.cats.functor

import org.scalatest.{Matchers, WordSpec}

class TreeFunctorSpec extends WordSpec with Matchers {

  "A Tree functor" should {

    import Tree._
    import Tree.Instances._
    import cats.syntax.functor._

    "map over single leaf" in {
      leaf(10).map(_ + 10) should be(leaf(20))
    }

    "map over complex tree" in {
      val tree = branch(branch(leaf(10), leaf(20)), leaf(30))
      val expected = branch(branch(leaf(20), leaf(30)), leaf(40))
      tree.map(_ + 10) should be(expected)
    }
  }
}
