package com.github.bhop.cats.traverse

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{Matchers, WordSpec}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class TraverseSpec extends WordSpec with Matchers with ScalaFutures {

  "A Traverse" should {

    import cats.Traverse

    "convert a list of futures into a future of a list using sequence" in {
      val input = List(Future(1), Future(2), Future(3), Future(4))
      import cats.instances.list._
      import cats.instances.future._
      Traverse[List].sequence(input).futureValue should be(List(1, 2, 3, 4))

      // or using syntax...
      import cats.syntax.traverse._
      input.sequence.futureValue should be(List(1, 2, 3, 4))
    }

    "convert a list of futures into a future of a list using traverse" in {
      val input = List(1, 2, 3, 4)
      import cats.instances.list._
      import cats.instances.future._
      Traverse[List].traverse(input)(Future(_)).futureValue should be(input)

      // or using syntax...
      import cats.syntax.traverse._
      input.traverse(Future(_)).futureValue should be(input)
    }
  }
}
