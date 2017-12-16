package com.github.bhop.cats.monoid

import org.scalatest.{Matchers, WordSpec}

class SuperAdderSpec extends WordSpec with Matchers {

  import SuperAdderSpec._

  "A SuperAdder" should {

    import SuperAdder._
    import cats.instances.int._

    "sum non empty list" in {
      add(List(1, 2, 3, 4, 5)) should be(15)
    }

    "return first item if list size is 1" in {
      add(List(5)) should be(5)
    }

    "return zero if empty list provided" in {
      add(List()) should be(0)
    }

    "sum non empty list of options" in {
      import cats.syntax.option._
      import cats.instances.option._
      add(List(1.some, none[Int], 5.some, 4.some)) should be(10.some)
    }

    "sum non empty list of orders" in {
      import OrderInstances._
      add(List(Order(10.0, 2.0), Order(25.0, 1.0), Order(100.0, 5.0))) should be(Order(135.0, 8.0))
    }
  }
}

object SuperAdderSpec {

  case class Order(totalCost: Double, quantity: Double)

  object OrderInstances {

    implicit val orderMonoid: cats.Monoid[Order] =
      new cats.Monoid[Order] {

        override def combine(x: Order, y: Order): Order =
          Order(
            totalCost = x.totalCost + y.totalCost,
            quantity = x.quantity + y.quantity
          )

        override def empty: Order = Order(totalCost = 0.0, quantity = 0.0)
      }
  }
}
