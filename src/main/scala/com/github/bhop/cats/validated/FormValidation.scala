package com.github.bhop.cats.validated

object FormValidation {

  case class User(name: String, age: Int)

  import cats.data.Validated
  type ErrorsOr[A] = Validated[Vector[String], A]
  type FormData = Map[String, String]

  def readUser(data: FormData): ErrorsOr[User] = {
    import cats.instances.vector._
    import cats.syntax.apply._
    (readName(data), readAge(data)).mapN(User.apply)
  }

  def readName(data: FormData): ErrorsOr[String] =
    getValue("name")(data)
      .andThen(name => nonBlank("name")(name))

  def readAge(data: FormData): ErrorsOr[Int] =
    getValue("age")(data)
      .andThen(age => parseInt("age")(age))
      .andThen(age => nonNegative("age")(age))

  def getValue(name: String)(data: FormData): ErrorsOr[String] =
    Validated.fromOption(data.get(name), ifNone = Vector(s"$name field not specified"))

  def parseInt(name: String)(value: String): ErrorsOr[Int] =
    Validated.catchOnly[NumberFormatException](value.toInt)
      .leftMap(_ => Vector(s"$name must be an integer"))

  def nonBlank(name: String)(value: String): ErrorsOr[String] = {
    import cats.syntax.validated._
    if (value.isEmpty) Vector(s"$name cannot be blank").invalid
    else value.valid
  }

  def nonNegative(name: String)(value: Int): ErrorsOr[Int] = {
    import cats.syntax.validated._
    if (value < 0) Vector(s"$name must be non-negative").invalid
    else value.valid
  }
}
