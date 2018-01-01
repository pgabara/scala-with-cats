package com.github.bhop.cats.casestudies.mapreduce

import scala.concurrent.{ExecutionContext, Future}

object MapReduce {

  import cats.Monoid

  def foldMap[A, B: Monoid](input: Vector[A])(func: A => B): B = {
    import cats.syntax.semigroup._
    input.map(func).foldLeft(Monoid[B].empty)(_ |+| _)
  }

  def parallelFoldMap[A, B: Monoid](input: Vector[A])(func: A => B)(implicit ec: ExecutionContext): Future[B] = {
    val numberOfCores = Runtime.getRuntime.availableProcessors
    val batchSize = (1.0 * input.size / numberOfCores).ceil.toInt
    val batches = input.grouped(if (batchSize > 0) batchSize else 1).toVector
    val results = Future.sequence(batches.map(batch => Future(foldMap(batch)(func))))
    results.map(foldMap(_)(identity))
  }

  def parallelFoldMap2[A, B: Monoid](input: Vector[A])(func: A => B)(implicit ec: ExecutionContext): Future[B] = {
    val numberOfCores = Runtime.getRuntime.availableProcessors
    val batchSize = (1.0 * input.size / numberOfCores).ceil.toInt

    import cats.syntax.traverse._
    import cats.syntax.foldable._
    import cats.instances.vector._
    import cats.instances.future._

    input
      .grouped(if (batchSize > 0) batchSize else 1)
      .toVector
      .traverse[Future, B](batch => Future(batch.foldMap(func)))
      .map(_.combineAll)
  }
}
