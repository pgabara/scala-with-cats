package com.github.bhop.cats.validated

import org.scalatest.{Matchers, WordSpec}

class FormValidationSpec extends WordSpec with Matchers {

  import FormValidation._

  "A Form Validation" when {

    import cats.syntax.validated._

    "getValue" should {

      "return valid with value" in {
        val data: FormData = Map("name" -> "admin", "age" -> "34")
        getValue("name")(data) should be("admin".valid)
      }

      "return invalid with error" in {
        val data: FormData = Map.empty
        getValue("name")(data) should be(Vector("name field not specified").invalid)
      }
    }

    "parseInt" should {

      "convert string to int and return it as a valid value" in {
        parseInt("age")("34") should be(34.valid)
      }

      "return invalid with error if value cannot be converted" in {
        parseInt("age")("foo") should be(Vector("age must be an integer").invalid)
      }
    }

    "nonBlank" should {

      "return value if not blank" in {
        nonBlank("name")("admin") should be("admin".valid)
      }

      "return invalid with an error message if value is blank" in {
        nonBlank("name")("") should be(Vector("name cannot be blank").invalid)
      }
    }

    "nonNegative" should {

      "return value if number is greater or equal to 0" in {
        nonNegative("age")(34) should be(34.valid)
      }

      "return invalid with an error message if number is less than 0" in {
        nonNegative("age")(-34) should be(Vector("age must be non-negative").invalid)
      }
    }

    "readUser" should {

      "return a user by giving valid form data" in {
        val data: FormData = Map("name" -> "admin", "age" -> "34")
        readUser(data) should be(User("admin", 34).valid)
      }

      "return errors if values not found" in {
        val data: FormData = Map.empty
        readUser(data) should be(Vector("name field not specified", "age field not specified").invalid)
      }

      "return errors if name is blank and age is a negative number" in {
        val data: FormData = Map("name" -> "", "age" -> "-34")
        readUser(data) should be(Vector("name cannot be blank", "age must be non-negative").invalid)
      }

      "return errors if age is not a number" in {
        val data: FormData = Map("name" -> "admin", "age" -> "foo")
        readUser(data) should be(Vector("age must be an integer").invalid)
      }
    }
  }
}
