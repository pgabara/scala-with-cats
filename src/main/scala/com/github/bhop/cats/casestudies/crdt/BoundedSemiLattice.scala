package com.github.bhop.cats.casestudies.crdt

import cats.Monoid

trait BoundedSemiLattice[A] extends Monoid[A] {

  override def combine(x: A, y: A): A

  override def empty: A
}

object BoundedSemiLattice {

  implicit val intInstance: BoundedSemiLattice[Int] = new BoundedSemiLattice[Int] {
    override def combine(x: Int, y: Int): Int = x max y
    override def empty: Int = 0
  }

  implicit def setInstance[A]: BoundedSemiLattice[Set[A]] = new BoundedSemiLattice[Set[A]] {
    override def empty: Set[A] = Set.empty[A]
    override def combine(x: Set[A], y: Set[A]): Set[A] = x union y
  }
}