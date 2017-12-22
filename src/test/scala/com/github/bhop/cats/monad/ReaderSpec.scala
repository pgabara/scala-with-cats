package com.github.bhop.cats.monad

import com.github.bhop.cats.Cat
import org.scalatest.{Matchers, WordSpec}

class ReaderSpec extends WordSpec with Matchers {

  "A Reader" should {

    import cats.data.Reader

    "create and run an instance" in {
      val catName: Reader[Cat, String] = Reader(_.name)
      catName.run(Cat("Garfield", age = 10, "ginger")) should be("Garfield")
    }

    "compose readers" in {
      val introduce = for {
        name  <- Reader[Cat, String](_.name)
        greet =  s"Hello $name"
        age   <- Reader[Cat, Int](_.age)
      } yield s"$greet. Age: $age"
      introduce.run(Cat("Garfield", age = 10, "ginger")) should be("Hello Garfield. Age: 10")
    }
  }
}