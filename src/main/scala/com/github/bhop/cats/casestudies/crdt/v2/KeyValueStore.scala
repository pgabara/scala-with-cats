package com.github.bhop.cats.casestudies.crdt.v2

trait KeyValueStore[F[_, _]] {

  def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]

  def get[K, V](f: F[K, V])(k: K): Option[V]

  def getOrElse[K, V](f: F[K, V])(k: K, default: V): V =
    get(f)(k).getOrElse(default)

  def values[K, V](f: F[K, V]): List[V]
}

object KeyValueStore {

  object Instances {

    implicit def mapInstance: KeyValueStore[Map] = new KeyValueStore[Map] {

      override def values[K, V](f: Map[K, V]): List[V] = f.values.toList

      override def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] = f + (k -> v)

      override def get[K, V](f: Map[K, V])(k: K): Option[V] = f.get(k)
    }
  }

  implicit class KeyValueStoreOps[F[_, _], K, V](f: F[K, V]) {

    def put(key: K, value: V)(implicit kvs: KeyValueStore[F]): F[K, V] =
      kvs.put(f)(key, value)

    def get(key: K)(implicit kvs: KeyValueStore[F]): Option[V] =
      kvs.get(f)(key)

    def getOrElse(key: K, default: V)(implicit kvs: KeyValueStore[F]): V =
      kvs.getOrElse(f)(key, default)

    def values(implicit kvs: KeyValueStore[F]): List[V] =
      kvs.values(f)
  }
}
