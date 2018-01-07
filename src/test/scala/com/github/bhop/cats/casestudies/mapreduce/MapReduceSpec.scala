package com.github.bhop.cats.casestudies.mapreduce

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{Matchers, WordSpec}

class MapReduceSpec extends WordSpec with Matchers with ScalaFutures {

  import MapReduce._

  "A MapReduce" when {

    "foldMap" should {

      "convert a vector of numbers to a vector of strings and join all elements" in {
        import cats.instances.string._
        foldMap(Vector(1, 2, 3, 4))(_.toString) should be("1234")
      }

      "sum up all elements providing a vector of numbers and identity function" in {
        import cats.instances.int._
        foldMap(Vector(1, 2, 3, 4))(identity) should be(10)
      }

      "return 0 providing an empty vector of numbers with identity function" in {
        import cats.instances.int._
        foldMap(Vector.empty[Int])(identity) should be(0)
      }
    }

    "parallelFoldMap" should {

      import scala.concurrent.ExecutionContext.Implicits.global

      "sum up all elements providing a vector of numbers and identity function" in {
        import cats.instances.int._
        parallelFoldMap(Vector(1, 2, 3, 4))(identity).futureValue should be(10)
      }

      "return 0 providing an empty vector of numbers with identity function" in {
        import cats.instances.int._
        parallelFoldMap(Vector.empty[Int])(identity).futureValue should be(0)
      }
    }

    "parallelFoldMap2" should {

      import scala.concurrent.ExecutionContext.Implicits.global

      "sum up all elements providing a vector of numbers and identity function" in {
        import cats.instances.int._
        parallelFoldMap2(Vector(1, 2, 3, 4))(identity).futureValue should be(10)
      }

      "return 0 providing an empty vector of numbers with identity function" in {
        import cats.instances.int._
        parallelFoldMap2(Vector.empty[Int])(identity).futureValue should be(0)
      }
    }
  }
}
