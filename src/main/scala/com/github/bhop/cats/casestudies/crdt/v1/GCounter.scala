package com.github.bhop.cats.casestudies.crdt.v1

import cats.Monoid
import cats.instances.list._
import cats.instances.map._
import cats.syntax.semigroup._
import cats.syntax.foldable._
import com.github.bhop.cats.casestudies.crdt.BoundedSemiLattice

final case class GCounter[A](counters: Map[String, A]) {

  def increment(machine: String, amount: A)(implicit m: Monoid[A]): GCounter[A] = {
    val counter = amount |+| counters.getOrElse(machine, amount)
    GCounter(counters + (machine -> counter))
  }

  def merge(that: GCounter[A])(implicit b: BoundedSemiLattice[A]): GCounter[A] =
    GCounter(this.counters |+| that.counters)

  def total(implicit m: Monoid[A]): A =
    counters.values.toList.combineAll
}
