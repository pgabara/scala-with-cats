package com.github.bhop.cats.typeclass

trait Printable[A] {

  def format(a: A): String

  def contramap[B](f: B => A): Printable[B] =
    b => format(f(b))
}

object Printable {

  def format[A](a: A)(implicit p: Printable[A]): String =
    p.format(a)

  object Syntax {

    implicit class PrintableOps[A](a: A) {
      def format(implicit p: Printable[A]): String = p.format(a)
    }
  }

  object Instances {

    implicit val stringPrintable: Printable[String] =
      a => a

    implicit val intPrintable: Printable[Int] =
      stringPrintable.contramap[Int](_.toString)

    implicit val booleanPrintable: Printable[Boolean] =
      stringPrintable.contramap[Boolean] {
        case true  => "yes"
        case false => "no"
      }
  }
}
