package com.github.bhop.cats.functor

sealed trait Tree[+A]

object Tree {

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  def leaf[A](value: A): Tree[A] = Leaf(value)

  object Instances {
    implicit val treeFunctor: cats.Functor[Tree] =
      new cats.Functor[Tree] {
        override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
          fa match {
            case Leaf(value) => Leaf(f(value))
            case Branch(left, right) => Branch(map(left)(f), map(right)(f))
          }
      }
  }
}
