package com.github.bhop.cats.casestudies.testing

import cats.Applicative

class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {

  def getTotalUptime(hostnames: List[String]): F[Int] = {
    import cats.syntax.traverse._
    import cats.syntax.functor._
    import cats.instances.list._
    hostnames.traverse(client.getUptime).map(_.sum)
  }
}
