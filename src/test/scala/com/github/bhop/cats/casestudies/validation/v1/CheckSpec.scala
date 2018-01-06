package com.github.bhop.cats.casestudies.validation.v1

import org.scalatest.{Matchers, WordSpec}
import cats.syntax.validated._

class CheckSpec extends WordSpec with Matchers {

  import Check._

  "A Check" when {

    "Pure" should {

      "be valid if check is valid" in {
        import cats.instances.string._
        val rule = Pure[String, Int](x => if (x > 5) x.valid else "Invalid number".invalid)
        rule(10).getOrElse(0) should be(10)
      }

      "be invalid if check is invalid" in {
        import cats.instances.string._
        val rule = Pure[String, Int](x => if (x > 5) x.valid else "Invalid number".invalid)
        rule(2).swap.getOrElse("") should be("Invalid number")
      }
    }

    "And" should {

      "be valid if both checks are valid" in {
        import cats.instances.list._
        val rule = And(
          Pure[List[String], Int](x => if(x > 5) x.valid else List("Invalid number").invalid),
          Pure[List[String], Int](x => if(x % 2 == 0) x.valid else List("Number is not even").invalid)
        )
        rule(10).getOrElse(0) should be(10)
      }

      "be invalid if at least one check is invalid" in {
        import cats.instances.list._
        val rule = And(
          Pure[List[String], Int](x => if(x > 5) x.valid else List("Invalid number").invalid),
          Pure[List[String], Int](x => if(x % 2 == 0) x.valid else List("Number is not even").invalid)
        )
        rule(1).swap.getOrElse(List.empty) should be(List("Invalid number", "Number is not even"))
        rule(7).swap.getOrElse(List.empty) should be(List("Number is not even"))
        rule(4).swap.getOrElse(List.empty) should be(List("Invalid number"))
      }
    }

    "Or" should {

      "be valid if both checks are valid" in {
        import cats.instances.list._
        val rule = Or(
          Pure[List[String], Int](x => if(x > 5) x.valid else List("Invalid number").invalid),
          Pure[List[String], Int](x => if(x % 2 == 0) x.valid else List("Number is not even").invalid)
        )
        rule(10).getOrElse(0) should be(10)
      }

      "be valid if one of checks is valid" in {
        import cats.instances.list._
        val rule = Or(
          Pure[List[String], Int](x => if(x > 5) x.valid else List("Invalid number").invalid),
          Pure[List[String], Int](x => if(x % 2 == 0) x.valid else List("Number is not even").invalid)
        )
        rule(7).getOrElse(0) should be(7)
        rule(4).getOrElse(0) should be(4)
      }

      "be invalid if both checks are invalid" in {
        import cats.instances.list._
        val rule = Or(
          Pure[List[String], Int](x => if(x > 5) x.valid else List("Invalid number").invalid),
          Pure[List[String], Int](x => if(x % 2 == 0) x.valid else List("Number is not even").invalid)
        )
        rule(1).swap.getOrElse(List.empty) should be(List("Invalid number", "Number is not even"))
      }
    }
  }
}
