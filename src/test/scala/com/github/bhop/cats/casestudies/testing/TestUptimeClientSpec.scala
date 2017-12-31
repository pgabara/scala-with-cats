package com.github.bhop.cats.casestudies.testing

import org.scalatest.{Matchers, WordSpec}

class TestUptimeClientSpec extends WordSpec with Matchers {

  "A testUptimeClient" should {

    "return uptime if host recognized" in {
      val hosts = Map("host1" -> 10, "host2" -> 6)
      new TestUptimeClient(hosts).getUptime("host1") should be(10)
    }

    "return 0 if host not recognized" in {
      val hosts = Map("host1" -> 10, "host2" -> 6)
      new TestUptimeClient(hosts).getUptime("host3") should be(0)
    }
  }
}
