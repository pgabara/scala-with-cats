package com.github.bhop.cats.monoid

object SuperAdder {

  def add[A](items: List[A])(implicit m: cats.Monoid[A]): A = {
    import cats.syntax.semigroup._
    items.foldLeft(m.empty)(_ |+| _)
  }
}
