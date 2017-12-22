package com.github.bhop.cats.monad

import org.scalatest.{Matchers, WordSpec}

class StateSpec extends WordSpec with Matchers {

  import cats.data.State

  "A State" should {

    "create a state instance" in {
      val state = State[Int, String](state => state -> s"Current state is: $state")
      state.runS(5).value should be(5)
      state.runA(5).value should be("Current state is: 5")
      state.run(5).value should be(5 -> "Current state is: 5")
    }

    "compose and transform state" in {
      val result = for {
        a <- State[Int, String](s => s + 1 -> s"Result of step 1: ${s + 1}")
        b <- State[Int, String](s => s * 2 -> s"Result of step 2: ${s * 2}")
      } yield a -> b
      result.runS(1).value should be(4)
      result.runA(1).value should be("Result of step 1: 2" -> "Result of step 2: 4")
    }

    "get state" in {
      State.get[Int].run(10).value should be(10 -> 10)
    }

    "update state and return unit as result" in {
      State.set[Int](30).run(10).value should be((30, ()))
    }

    "ignore state and return result" in {
      State.pure[Int, String]("Result").run(10).value should be(10 -> "Result")
    }

    "extract the state via transformation function" in {
      State.inspect[Int, String](_ + "!").run(10).value should be(10 -> "10!")
    }

    "modify state returning unit as a result" in {
      State.modify[Int](_ + 1).run(10).value should be((11, ()))
    }

    "combine it all together" in {
      import State._
      val program: State[Int, (Int, Int, Int)] =
        for {
          a <- get[Int]
          _ <- set[Int](a + 1)
          b <- get[Int]
          _ <- modify[Int](_ + 1)
          c <- inspect[Int, Int](_ * 1000)
        } yield (a, b, c)
      val (state, result) = program.run(1).value
      state should be(3)
      result should be((1, 2, 3000))
    }
  }
}
