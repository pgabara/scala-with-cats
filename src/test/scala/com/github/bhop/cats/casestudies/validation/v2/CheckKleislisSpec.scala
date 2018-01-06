package com.github.bhop.cats.casestudies.validation.v2

import cats.data.{Kleisli, NonEmptyList}
import org.scalatest.{Matchers, WordSpec}

import cats.instances.either._

import cats.syntax.apply._
import cats.syntax.either._

class CheckKleislisSpec extends WordSpec with Matchers {

  "A Check" when {

    type Errors = NonEmptyList[String]

    type Result[A] = Either[Errors, A]

    type Check[A, B] = Kleisli[Result, A, B]

    def check[A, B](func: A => Result[B]): Check[A, B] =
      Kleisli(func)

    def checkPred[A](pred: Predicate[Errors, A]): Check[A, A] =
      Kleisli[Result, A, A](pred.run)

    "a username must contain at least four characters and consist entirely of alphanumeric characters" should {

      val longerThan3 = Predicate.lift[Errors, String](
        error = error("Must be longer than 3 characters"), func = _.length > 3)

      val alphanumeric = Predicate.lift[Errors, String](
        error = error("Must be all alphanumeric characters"), func = _.forall(_.isLetterOrDigit))

      val check = checkPred(longerThan3 and alphanumeric)

      "be valid for correct data" in {
        check("admin18") should be("admin18".asRight)
      }

      "be invalid for incorrect data" in {
        check("abc") should be(error("Must be longer than 3 characters").asLeft)
        check("admin18#") should be(error("Must be all alphanumeric characters").asLeft)
        check("ab#") should be(NonEmptyList.fromListUnsafe(
          List("Must be longer than 3 characters", "Must be all alphanumeric characters")).asLeft)
      }
    }

    "an email must contain an @ sign, left part must not be empty, right part must have at least 3 chars and have a dot char" should {

      def contains(c: Char) = Predicate.lift[Errors, String](
        error = error(s"Must contain the character $c"), func = _.contains(c))

      def longerThan(n: Int) = Predicate.lift[Errors, String](
        error = error(s"Must be longer than $n characters"), func = _.length > n)

      val splitEmail: Check[String, (String, String)] = check {
        _.split('@') match {
          case Array(name, domain) => (name, domain).asRight
          case _ => NonEmptyList("Must contain a single @ character", Nil).asLeft
        }
      }

      val checkLeft: Check[String, String] =
        checkPred(longerThan(0))

      val checkRight: Check[String, String] =
        checkPred(longerThan(3) and contains('.'))

      val joinEmail: Check[(String, String), String] =
        check {
          case (l, r) =>
            (checkLeft(l), checkRight(r)).mapN(_ + "@" + _)
        }

      val checkEmail: Check[String, String] = splitEmail andThen joinEmail

      "be valid for correct data" in {
        checkEmail("admin@domain.com") should be("admin@domain.com".asRight)
      }

      "be invalid for incorrect data" in {
        checkEmail("admin[at]domain.com") should be(NonEmptyList("Must contain a single @ character", Nil).asLeft)
      }
    }
  }

  def error(s: String): NonEmptyList[String] = NonEmptyList(s, List.empty)
}
