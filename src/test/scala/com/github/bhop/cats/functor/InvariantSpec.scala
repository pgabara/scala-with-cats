package com.github.bhop.cats.functor

import java.util.Date

import org.scalatest.{Matchers, WordSpec}

class InvariantSpec extends WordSpec with Matchers {

  "An Invariant functor" should {

    "create a Monoid working on dates from a Monoid that operates on long numbers" in {
      import cats.Monoid
      import cats.syntax.invariant._
      import cats.syntax.semigroup._
      import cats.instances.monoid._
      import cats.instances.long._
      implicit val dateMonoid: Monoid[Date] = Monoid[Long].imap[Date](new Date(_))(_.getTime)
      dateMonoid.empty should be(new Date(0))
      new Date(100000) |+| new Date(100000) should be(new Date(200000))
    }
  }
}
