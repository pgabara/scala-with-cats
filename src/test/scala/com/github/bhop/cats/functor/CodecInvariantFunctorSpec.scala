package com.github.bhop.cats.functor

import com.github.bhop.cats.Box
import org.scalatest.{Matchers, WordSpec}

class CodecInvariantFunctorSpec extends WordSpec with Matchers {

  "A Codec as invariant functor" should {

    import Codec.Instances._

    "encode a box instance" in {
      import Box.Instances._
      Codec.encode(Box(5)) should be("5")
    }

    "decode to a box instance" in {
      import Box.Instances._
      Codec.decode[Box[Int]]("5") should be(Box(5))
    }

    "encode a double value" in {
      Codec.encode(10.5) should be("10.5")
    }

    "decode to a double value" in {
      Codec.decode[Double]("10.5") should be(10.5)
    }
  }
}
