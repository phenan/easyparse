package com.phenan.easyparse

sealed trait Parser [E, T] {
  def map [U] (f: T => U): Parser[E, U] = ParserImpl.MappedParser(this, f)
  def collect [U] (f: PartialFunction[T, U]): Parser[E, U] = ParserImpl.PartialMappedParser(this, f)
  def filter (f: T => Boolean): Parser[E, T] = ParserImpl.FilteredParser(this, f)

  // This method is used only for type checking.
  def unapply (arg: String): Option[T] = None
}

private[easyparse] object ParserImpl {

  case class PureParser[E, T] (value: T) extends Parser[E, T]

  case class ChoiceParser[E, T] (parsers: Seq[Parser[E, T]]) extends Parser[E, T]

  case class FilteredParser[E, T] (parser: Parser[E, T], filter: T => Boolean) extends Parser[E, T]

  case class MappedParser[E, T1, T2] (parser: Parser[E, T1], f: T1 => T2) extends Parser[E, T2]

  case class PartialMappedParser[E, T1, T2] (parser: Parser[E, T1], f: PartialFunction[T1, T2]) extends Parser[E, T2]

  case class SequentialParser[E, T1, T2] (parser1: Parser[E, T1], parser2: Parser[E, T2]) extends Parser[E, (T1, T2)]

  case class PrefixedParser[E, T] (prefix: Parser[E, _], parser: Parser[E, T]) extends Parser[E, T]

  case class PostfixedParser[E, T] (parser: Parser[E, T], postfix: Parser[E, _]) extends Parser[E, T]

  case class LexicalParser[E, T] (lexer: Lexer[T]) extends Parser[E, T]

  case class TokensParser[E] (tokens: Seq[E]) extends Parser[E, Seq[E]]

}
