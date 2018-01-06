package com.github.bhop.cats.casestudies.crdt.v2

import cats.Monoid

import cats.instances.list._

import cats.syntax.semigroup._
import cats.syntax.foldable._

import com.github.bhop.cats.casestudies.crdt.BoundedSemiLattice

trait GCounter[F[_,_], K, V] {

  def increment(f: F[K, V])(k: K, v: V)(implicit m: Monoid[V]): F[K, V]

  def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]

  def total(f: F[K, V])(implicit m: Monoid[V]): V
}

object GCounter {

  def apply[F[_, _], K, V](implicit c: GCounter[F, K, V]): GCounter[F, K, V] = c

  import KeyValueStore._

  object Instances {

    implicit def gCounterInstance[F[_, _], K, V](implicit kvs: KeyValueStore[F], km: Monoid[F[K, V]]): GCounter[F, K, V] =
      new GCounter[F, K, V] {

        override def increment(f: F[K, V])(k: K, v: V)(implicit m: Monoid[V]): F[K, V] =
          f.put(k, f.getOrElse(k, m.empty) |+| v)

        override def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V] =
          f1 |+| f2

        override def total(f: F[K, V])(implicit m: Monoid[V]): V =
          f.values.combineAll
      }
  }
}
