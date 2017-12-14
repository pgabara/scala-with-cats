package com.github.bhop.cats

import com.github.bhop.cats.typeclass.Json._
import com.github.bhop.cats.typeclass.Printable

case class Cat(name: String, age: Int, color: String)

object Cat {

  object Instances {

    implicit val catJsonWriter: JsonWriter[Cat] =
      cat => JsObject(Map(
        "name"  -> JsString(cat.name),
        "age"   -> JsNumber(cat.age),
        "color" -> JsString(cat.color)
      ))

    implicit val catPrintable: Printable[Cat] =
      cat => s"${cat.name} is a ${cat.age} year-old ${cat.color} cat."

    import cats.Show
    implicit val catShow: Show[Cat] =
      Show.show(cat => s"${cat.name} is a ${cat.age} year-old ${cat.color} cat.")

    import cats.Eq
    implicit val catEq: Eq[Cat] =
      Eq.instance(_ == _)
  }
}
