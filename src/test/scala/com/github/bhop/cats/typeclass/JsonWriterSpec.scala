package com.github.bhop.cats.typeclass

import org.scalatest.{Matchers, WordSpec}
import com.github.bhop.cats.Cat
import com.github.bhop.cats.typeclass.Json._

class JsonWriterSpec extends WordSpec with Matchers {

  "A JsonWriter" should {

    import Json.Syntax._
    import Json.Instances._

    "transform a cat to json" in {
      import Cat.Instances._

      val expected = JsObject(Map(
        "name"   -> JsString("Max"),
        "age"    -> JsNumber(4),
        "color" -> JsString("black")
      ))

      Cat(name = "Max", age = 4, color = "black").toJson should be(expected)
    }

    "transform a list to json" in {
      val expected = JsArray(JsString("scala") :: JsString("cats") :: Nil)
      List("scala", "cats").toJson should be(expected)
    }
  }
}
