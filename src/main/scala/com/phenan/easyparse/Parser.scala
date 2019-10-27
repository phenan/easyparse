package com.phenan.easyparse

sealed trait Parser [E, +T] {
  def map [U] (f: T => U): Parser[E, U] = ParserImpl.MappedParser(this, f)
  def collect [U] (f: PartialFunction[T, U]): Parser[E, U] = ParserImpl.PartialMappedParser(this, f)
  def filter (f: T => Boolean): Parser[E, T] = ParserImpl.FilteredParser(this, f)

  // This method is used only for type checking.
  def unapply (arg: String): Option[T] = None

  private[easyparse] def acceptVisitor [R[_, +_]] (visitor: ParserImpl.ParserVisitor[R]): R[E, T]
}

private[easyparse] object ParserImpl {

  trait ParserVisitor [R[_, +_]] {
    def visit [E, T] (parser: Parser[E, T]): R[E, T] = parser.acceptVisitor(this)

    def visitPureParser [E, T] (parser: PureParser[E, T]): R[E, T]
    def visitChoiceParser [E, T] (parser: ChoiceParser[E, T]): R[E, T]
    def visitFilteredParser [E, T] (parser: FilteredParser[E, T]): R[E, T]
    def visitMappedParser [E, T1, T2] (parser: MappedParser[E, T1, T2]): R[E, T2]
    def visitPartialMappedParser [E, T1, T2] (parser: PartialMappedParser[E, T1, T2]): R[E, T2]
    def visitSequentialParser [E, T1, T2] (parser: SequentialParser[E, T1, T2]): R[E, (T1, T2)]
    def visitPrefixedParser [E, T] (parser: PrefixedParser[E, T]): R[E, T]
    def visitPostfixedParser [E, T] (parser: PostfixedParser[E, T]): R[E, T]
    def visitLexicalParser [E, T] (parser: LexicalParser[E, T]): R[E, T]
    def visitTokensParser [E] (parser: TokensParser[E]): R[E, Seq[E]]
  }

  case class PureParser[E, T] (value: T) extends Parser[E, T] {
    def acceptVisitor [R[_, +_]] (visitor: ParserImpl.ParserVisitor[R]): R[E, T] = visitor.visitPureParser(this)
  }

  case class ChoiceParser[E, T] (parsers: Seq[Parser[E, T]]) extends Parser[E, T] {
    def acceptVisitor [R[_, +_]] (visitor: ParserImpl.ParserVisitor[R]): R[E, T] = visitor.visitChoiceParser(this)
  }

  case class FilteredParser[E, T] (parser: Parser[E, T], filter: T => Boolean) extends Parser[E, T] {
    def acceptVisitor [R[_, +_]] (visitor: ParserImpl.ParserVisitor[R]): R[E, T] = visitor.visitFilteredParser(this)
  }

  case class MappedParser[E, T1, T2] (parser: Parser[E, T1], f: T1 => T2) extends Parser[E, T2] {
    def acceptVisitor [R[_, +_]] (visitor: ParserImpl.ParserVisitor[R]): R[E, T2] = visitor.visitMappedParser(this)
  }

  case class PartialMappedParser[E, T1, T2] (parser: Parser[E, T1], f: PartialFunction[T1, T2]) extends Parser[E, T2] {
    def acceptVisitor [R[_, +_]] (visitor: ParserImpl.ParserVisitor[R]): R[E, T2] = visitor.visitPartialMappedParser(this)
  }

  case class SequentialParser[E, T1, T2] (parser1: Parser[E, T1], parser2: Parser[E, T2]) extends Parser[E, (T1, T2)] {
    def acceptVisitor [R[_, +_]] (visitor: ParserImpl.ParserVisitor[R]): R[E, (T1, T2)] = visitor.visitSequentialParser(this)
  }

  case class PrefixedParser[E, T] (prefix: Parser[E, _], parser: Parser[E, T]) extends Parser[E, T] {
    def acceptVisitor [R[_, +_]] (visitor: ParserImpl.ParserVisitor[R]): R[E, T] = visitor.visitPrefixedParser(this)
  }

  case class PostfixedParser[E, T] (parser: Parser[E, T], postfix: Parser[E, _]) extends Parser[E, T] {
    def acceptVisitor [R[_, +_]] (visitor: ParserImpl.ParserVisitor[R]): R[E, T] = visitor.visitPostfixedParser(this)
  }

  case class LexicalParser[E, T] (lexer: Lexer[T]) extends Parser[E, T] {
    def acceptVisitor [R[_, +_]] (visitor: ParserImpl.ParserVisitor[R]): R[E, T] = visitor.visitLexicalParser(this)
  }

  case class TokensParser[E] (tokens: Seq[E]) extends Parser[E, Seq[E]] {
    def acceptVisitor [R[_, +_]] (visitor: ParserImpl.ParserVisitor[R]): R[E, Seq[E]] = visitor.visitTokensParser(this)
  }
}
