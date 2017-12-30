package com.github.bhop.cats.foldable

import org.scalatest.{Matchers, WordSpec}

class FoldableSpec extends WordSpec with Matchers {

  "A Foldable" should {

    import cats.Foldable

    "foldLeft over a list" in {
      import cats.instances.list._
      Foldable[List].foldLeft(List(1, 2, 3, 4), 0)(_ + _) should be(10)
    }

    "foldLeft over an option" in {
      import cats.instances.option._
      import cats.syntax.option._
      Foldable[Option].foldLeft(5.some, 5)(_ + _) should be(10)
      Foldable[Option].foldLeft(none[Int], 5)(_ + _) should be(5)
    }

    "foldRight over a stream (stack safe)" in {
      import cats.Eval
      import cats.instances.stream._
      val sum = Foldable[Stream].foldRight((1 to 10).toStream, Eval.now(0)) { (x, a) => a.map(_ + x) }
      sum.value should be(55)
    }

    "combineAll over a list of numbers" in {
      import cats.instances.int._
      import cats.instances.list._
      Foldable[List].combineAll(List(1, 2, 3, 4)) should be(10)
    }

    "foldMap over a list" in {
      import cats.instances.string._
      import cats.instances.list._
      Foldable[List].foldMap(List(1, 2, 3))(_.toString) should be("123")
    }
  }
}
