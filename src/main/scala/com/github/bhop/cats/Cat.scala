package com.github.bhop.cats

import com.github.bhop.cats.typeclass.Json._

case class Cat(name: String, age: Int, color: String)

object Cat {

  object Instances {

    implicit val catJsonWriter: JsonWriter[Cat] =
      cat => JsObject(Map(
        "name"  -> JsString(cat.name),
        "age"   -> JsNumber(cat.age),
        "color" -> JsString(cat.color)
      ))
  }
}
