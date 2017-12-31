package com.github.bhop.cats.casestudies.testing

import cats.Id

import scala.concurrent.Future

trait UptimeClient[F[_]] {

  def getUptime(hostname: String): F[Int]
}

trait AsyncUptimeClient extends UptimeClient[Future] {

  def getUptime(hostname: String): Future[Int]
}

trait SyncUptimeClient extends UptimeClient[Id] {

  def getUptime(hostname: String): Id[Int]
}

class TestUptimeClient(hosts: Map[String, Int]) extends SyncUptimeClient {

  override def getUptime(hostname: String): Id[Int] = hosts.getOrElse(hostname, 0)
}