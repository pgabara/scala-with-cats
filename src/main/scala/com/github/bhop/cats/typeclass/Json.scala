package com.github.bhop.cats.typeclass

sealed trait Json

object Json {

  case class JsObject(value: Map[String, Json]) extends Json
  case class JsString(value: String) extends Json
  case class JsNumber(value: Double) extends Json
  case class JsBoolean(value: Boolean) extends Json
  case class JsArray(value: List[Json]) extends Json
  case object JsNull extends Json

  trait JsonWriter[A] {
    def write(a: A): Json
  }

  object Syntax {
    implicit class JsonWriterOps[A](a: A) {
      def toJson(implicit w: JsonWriter[A]): Json = w.write(a)
    }
  }

  object Instances {

    implicit val stringJsonWriter: JsonWriter[String] =
      JsString(_)

    implicit def listJsonWriter[A](implicit w: JsonWriter[A]): JsonWriter[List[A]] =
      list => JsArray(list.map(w.write))
  }
}