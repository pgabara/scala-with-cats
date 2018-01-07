package com.github.bhop.cats.casestudies.validation.v2

import org.scalatest.{Matchers, WordSpec}

import cats.data.NonEmptyList

import cats.syntax.apply._
import cats.syntax.validated._

class CheckSpec extends WordSpec with Matchers {

  "A Check" when {

    "map" should {

      import cats.instances.string._
      val check = Check(Predicate.lift[String, Int](error = "Boom!", func = _ > 5))

      "transform valid value" in {
        check.map(_.toString)(10) should be("10".valid)
      }

      "ignore if invalid value" in {
        check.map(_.toString)(1) should be("Boom!".invalid)
      }
    }

    "a number should be greater than 10 or even" should {

      val longerThan10 = Predicate.lift[Errors, Int](
        error = error("Must be longer than 10 characters"), func = _ > 10)

      val isEven = Predicate.lift[Errors, Int](
        error = error("Number mus be even"), func = _ % 2 == 0)

      val check = Check(longerThan10 or isEven)

      "be valid for correct numbers" in {
        check(4) should be(4.valid)
        check(13) should be(13.valid)
        check(16) should be(16.valid)
      }

      "be invalid for incorrect numbers" in {
        check(3) should be(NonEmptyList.fromListUnsafe(
          List("Must be longer than 10 characters", "Number mus be even")).invalid)
      }
    }

    "a username must contain at least four characters and consist entirely of alphanumeric characters" should {

      val longerThan3 = Predicate.lift[Errors, String](
        error = error("Must be longer than 3 characters"), func = _.length > 3)

      val alphanumeric = Predicate.lift[Errors, String](
        error = error("Must be all alphanumeric characters"), func = _.forall(_.isLetterOrDigit))

      val check = Check(longerThan3 and alphanumeric)

      "be valid for correct data" in {
        check("admin18") should be("admin18".valid)
      }

      "be invalid for incorrect data" in {
        check("abc") should be(error("Must be longer than 3 characters").invalid)
        check("admin18#") should be(error("Must be all alphanumeric characters").invalid)
        check("ab#") should be(NonEmptyList.fromListUnsafe(
          List("Must be longer than 3 characters", "Must be all alphanumeric characters")).invalid)
      }
    }

    "an email must contain an @ sign, left part must not be empty, right part must have at least 3 chars and have a dot char" should {

      def contains(c: Char) = Predicate.lift[Errors, String](
        error = error(s"Must contain the character $c"), func = _.contains(c))

      def longerThan(n: Int) = Predicate.lift[Errors, String](
        error = error(s"Must be longer than $n characters"), func = _.length > n)

      val splitEmail: Check[Errors, String, (String, String)] = Check {
        _.split('@') match {
          case Array(name, domain) => (name, domain).validNel
          case _ => "Must contain a single @ character".invalidNel
        }
      }

      val checkLeft: Check[Errors, String, String] = Check(longerThan(0))

      val checkRight: Check[Errors, String, String] = Check(longerThan(3) and contains('.'))

      val joinEmail: Check[Errors, (String, String), String] =
        Check {
          case (l, r) => (checkLeft(l), checkRight(r)).mapN(_ + "@" + _)
        }

      val check: Check[Errors, String, String] = splitEmail andThen joinEmail

      "be valid for correct data" in {
        check("admin@domain.com") should be("admin@domain.com".valid)
      }

      "be invalid for incorrect data" in {
        check("admin[at]domain.com") should be(NonEmptyList("Must contain a single @ character", Nil).invalid)
      }
    }
  }

  type Errors = NonEmptyList[String]

  def error(s: String): NonEmptyList[String] = NonEmptyList(s, List.empty)
}
