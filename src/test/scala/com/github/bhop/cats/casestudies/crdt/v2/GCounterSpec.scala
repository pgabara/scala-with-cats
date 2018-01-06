package com.github.bhop.cats.casestudies.crdt.v2

import org.scalatest.{Matchers, WordSpec}

import com.github.bhop.cats.casestudies.crdt.BoundedSemiLattice

class GCounterSpec extends WordSpec with Matchers {

  "A GCounter" when {

    "Map instance" should {

      import cats.instances.map._
      import GCounter.Instances._
      import KeyValueStore.Instances._

      "calculate total amount" in {
        import cats.instances.int._
        val mapCounter = GCounter[Map, String, Int]
        mapCounter.total(Map("m1" -> 5, "m2" -> 9)) should be(14)
      }

      "merge two counters" in {
        import BoundedSemiLattice._
        val mapCounter = GCounter[Map, String, Int]
        val f1 = Map("m1" -> 0, "m2" -> 5)
        val f2 = Map("m1" -> 3, "m2" -> 1)
        mapCounter.merge(f1, f2) should be(Map("m1" -> 3, "m2" -> 5))
      }

      "increment a counter for a single machine" in {
        import cats.instances.int._
        val mapCounter = GCounter[Map, String, Int]
        mapCounter.increment(Map("m1" -> 3))("m1", 5) should be(Map("m1" -> 8))
      }
    }
  }
}
