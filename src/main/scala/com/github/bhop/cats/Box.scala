package com.github.bhop.cats

import com.github.bhop.cats.functor.Codec
import com.github.bhop.cats.typeclass.Printable

case class Box[A](value: A)

object Box {

  object Instances {

    implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
      p.contramap(_.value)

    implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] =
      Codec.Instances.stringCodec.imap[Box[A]](
        dec = value => Box(c.decode(value)),
        enc = box => c.encode(box.value)
      )
  }
}
