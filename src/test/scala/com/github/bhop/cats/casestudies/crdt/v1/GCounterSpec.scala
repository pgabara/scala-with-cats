package com.github.bhop.cats.casestudies.crdt.v1

import org.scalatest.{Matchers, WordSpec}

class GCounterSpec extends WordSpec with Matchers {

  "A GCounter" should {

    "calculate total amount" in {
      import cats.instances.int._
      GCounter(Map("m1" -> 10, "m2" -> 4, "m3" -> 6)).total should be(20)

      import cats.instances.set._
      GCounter(Map("m1" -> Set(10), "m2" -> Set(3))).total should be(Set(10, 3))
    }

    "merge two counters" in {
      import com.github.bhop.cats.casestudies.crdt.BoundedSemiLattice._
      val c1 = GCounter(Map("m1" -> 5, "m2" -> 0))
      val c2 = GCounter(Map("m1" -> 0, "m2" -> 5))
      c1.merge(c2) should be(GCounter(Map("m1" -> 5, "m2" -> 5)))

      val c3 = GCounter[Set[Int]](Map("m1" -> Set(2), "m2" -> Set.empty))
      val c4 = GCounter[Set[Int]](Map("m1" -> Set.empty, "m2" -> Set(5)))
      c3.merge(c4) should be(GCounter(Map("m1" -> Set(2), "m2" -> Set(5))))
    }

    "increment a counter for a single machine" in {
      import cats.instances.int._
      GCounter(Map("m1" -> 2)).increment("m1", 5) should be(GCounter(Map("m1" -> 7)))

      import cats.instances.set._
      GCounter(Map("m1" -> Set(2))).increment("m1", Set(4)) should be(GCounter(Map("m1" -> Set(2, 4))))
    }
  }
}
