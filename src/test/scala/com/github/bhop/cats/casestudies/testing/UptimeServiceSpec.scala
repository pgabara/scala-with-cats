package com.github.bhop.cats.casestudies.testing

import org.scalatest.{Matchers, WordSpec}

class UptimeServiceSpec extends WordSpec with Matchers {

  "A UptimeService" should {

    "sum up all uptimes" in {
      val hosts = Map("host1" -> 10, "host2" -> 6, "host3" -> 2)
      val uptimeClient = new TestUptimeClient(hosts)
      new UptimeService(uptimeClient).getTotalUptime(hosts.keys.toList) should be(18)
    }
  }
}
