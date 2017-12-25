package com.github.bhop.cats.transformer

import scala.concurrent.{ExecutionContext, Future}

object Autobots {

  import cats.data.EitherT
  type Response[A] = EitherT[Future, String, A]

  val powerLevels: Map[String, Int] = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  def getPowerLevel(autobot: String)(implicit ec: ExecutionContext): Response[Int] = {
    import cats.instances.future._
    powerLevels.get(autobot) match {
      case Some(level) => EitherT.right[String](Future.successful(level))
      case None => EitherT.left[Int](Future.successful(s"$autobot unreachable"))
    }
  }

  def canSpecialMove(autobotA: String, autobotB: String)(implicit ec: ExecutionContext): Response[Boolean] = {
    import cats.instances.future._
    for {
      powerA <- getPowerLevel(autobotA)
      powerB <- getPowerLevel(autobotB)
    } yield (powerA + powerB) > 15
  }

  def tacticalReport(autobotA: String, autobotB: String)(implicit ec: ExecutionContext): Future[String] = {
    canSpecialMove(autobotA, autobotB).value.map {
      case Left(error)  => error
      case Right(true)  => s"$autobotA and $autobotB are ready to roll out!"
      case Right(false) => s"$autobotA and $autobotB need a recharge."
    }
  }
}
