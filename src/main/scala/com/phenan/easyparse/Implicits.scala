package com.phenan.easyparse

trait Implicits {
  implicit class StringContextExtension (sc: StringContext) {
    object l {
      def unapplySeq(in: String): Option[Seq[String]] = sc.s.unapplySeq(in)
    }
    object p {
      def unapplySeq(in: String): Option[Seq[String]] = sc.s.unapplySeq(in)
    }
  }
}
