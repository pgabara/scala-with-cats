package com.github.bhop.cats.monad

object FoldRightWithEvalMonad {

  import cats.Eval

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(foldRight(tail, acc)(fn)).map(fn(head, _))
      case _ =>
        Eval.now(acc)
    }
}
