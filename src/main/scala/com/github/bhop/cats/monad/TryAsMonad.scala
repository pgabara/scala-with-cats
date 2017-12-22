package com.github.bhop.cats.monad

object TryAsMonad {

  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  def leaf[A](value: A): Tree[A] = Leaf(value)

  implicit val treeMonad: cats.Monad[Tree] =
    new cats.Monad[Tree] {

      override def pure[A](x: A): Tree[A] = leaf(x)

      override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] =
        fa match {
          case Leaf(v) => f(v)
          case Branch(left, right) => branch(flatMap(left)(f), flatMap(right)(f))
        }

      override def tailRecM[A, B](arg: A)(func: A => Tree[Either[A, B]]): Tree[B] = {
        @annotation.tailrec
        def loop(open: List[Tree[Either[A, B]]], closed: List[Tree[B]]): List[Tree[B]] =
          open match {
            case Branch(l, r) :: next =>
              l match {
                case Branch(_, _) =>
                  loop(l :: r :: next, closed)
                case Leaf(Left(value)) =>
                  loop(func(value) :: r :: next, closed)
                case Leaf(Right(value)) =>
                  loop(r :: next, pure(value) :: closed)
              }
            case Leaf(Left(value)) :: next =>
              loop(func(value) :: next, closed)
            case Leaf(Right(value)) :: next =>
              closed match {
                case head :: tail =>
                  loop(next, Branch(head, pure(value)) :: tail)
                case Nil =>
                  loop(next, pure(value) :: closed)
              }
            case Nil =>
              closed
          }

        loop(List(func(arg)), Nil).head
      }
    }
}
