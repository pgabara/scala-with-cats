package com.github.bhop.cats.monad

import org.scalatest.{Matchers, WordSpec}

class LoginSystemWithReaderMonadSpec extends WordSpec with Matchers {

  import LoginSystemWithReaderMonad._

  "A Login System" should {

    "find existing user" in {
      import cats.syntax.option._
      findUser(id = 1).run(Db(users = Map(1 -> "admin"))) should be("admin".some)
    }

    "return none if user does not exist" in {
      import cats.syntax.option._
      findUser(id = 1).run(Db()) should be(none[String])
    }

    "return true if password and username are correct" in {
      checkPassword("admin", "admin").run(Db(passwords = Map("admin" -> "admin"))) should be(true)
    }

    "return false if password and username are incorrect" in {
      checkPassword("admin", "admin").run(Db(passwords = Map("admin" -> "12345"))) should be(false)
    }

    "return false if username does not exist" in {
      checkPassword("admin", "admin").run(Db(passwords = Map("root" -> "root"))) should be(false)
    }

    "return true if user exist and password is correct" in {
      checkLogin(1, "admin").run(Db(users = Map(1 -> "admin"), passwords = Map("admin" -> "admin"))) should be(true)
    }

    "return false if user does exist" in {
      checkLogin(1, "admin").run(Db(users = Map(2 -> "admin"), passwords = Map("admin" -> "admin"))) should be(false)
    }

    "return false if user has different password" in {
      checkLogin(1, "admin").run(Db(users = Map(1 -> "admin"), passwords = Map("admin" -> "12345"))) should be(false)
    }
  }
}
