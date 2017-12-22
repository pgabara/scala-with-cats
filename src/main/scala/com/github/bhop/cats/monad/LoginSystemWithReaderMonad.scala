package com.github.bhop.cats.monad

object LoginSystemWithReaderMonad {

  case class Db(users: Map[Int, String] = Map.empty, passwords: Map[String, String] = Map.empty)

  import cats.data.Reader
  type DbReader[A] = Reader[Db, A]

  def findUser(id: Int): DbReader[Option[String]] =
    Reader(_.users.get(id))

  def checkPassword(user: String, password: String): DbReader[Boolean] =
    Reader(_.passwords.get(user).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] = {
    import cats.syntax.applicative._
    for {
      user <- findUser(userId)
      isPasswordCorrect <- user.map(username => checkPassword(username, password)).getOrElse(false.pure[DbReader])
    } yield isPasswordCorrect
  }
}
