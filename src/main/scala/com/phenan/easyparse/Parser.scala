package com.phenan.easyparse

sealed trait Parser [E, +T] {


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

  class PureParser[E, T] (val value: T) extends Parser[E, T] {
    def acceptVisitor [R[_, +_]] (visitor: ParserImpl.ParserVisitor[R]): R[E, T] = visitor.visitPureParser(this)
  }

  class ChoiceParser[E, T] (ps: => Seq[Parser[E, T]]) extends Parser[E, T] {
    lazy val parsers: Seq[Parser[E, T]] = ps
    def acceptVisitor [R[_, +_]] (visitor: ParserImpl.ParserVisitor[R]): R[E, T] = visitor.visitChoiceParser(this)
  }

  class FilteredParser[E, T] (p: => Parser[E, T], f: T => Boolean) extends Parser[E, T] {
    lazy val parser: Parser[E, T] = p
    lazy val filter: T => Boolean = f
    def acceptVisitor [R[_, +_]] (visitor: ParserImpl.ParserVisitor[R]): R[E, T] = visitor.visitFilteredParser(this)
  }

  class MappedParser[E, T1, T2] (p: => Parser[E, T1], f: T1 => T2) extends Parser[E, T2] {
    lazy val parser: Parser[E, T1] = p
    lazy val mapping: T1 => T2 = f
    def acceptVisitor [R[_, +_]] (visitor: ParserImpl.ParserVisitor[R]): R[E, T2] = visitor.visitMappedParser(this)
  }

  class PartialMappedParser[E, T1, T2] (p: => Parser[E, T1], f: PartialFunction[T1, T2]) extends Parser[E, T2] {
    lazy val parser: Parser[E, T1] = p
    lazy val mapping: PartialFunction[T1, T2] = f
    def acceptVisitor [R[_, +_]] (visitor: ParserImpl.ParserVisitor[R]): R[E, T2] = visitor.visitPartialMappedParser(this)
  }

  class SequentialParser[E, T1, T2] (p1: => Parser[E, T1], p2: => Parser[E, T2]) extends Parser[E, (T1, T2)] {
    lazy val parser1: Parser[E, T1] = p1
    lazy val parser2: Parser[E, T2] = p2
    def acceptVisitor [R[_, +_]] (visitor: ParserImpl.ParserVisitor[R]): R[E, (T1, T2)] = visitor.visitSequentialParser(this)
  }

  class PrefixedParser[E, T] (p1: => Parser[E, _], p2: => Parser[E, T]) extends Parser[E, T] {
    lazy val prefix: Parser[E, _] = p1
    lazy val parser: Parser[E, T] = p2
    def acceptVisitor [R[_, +_]] (visitor: ParserImpl.ParserVisitor[R]): R[E, T] = visitor.visitPrefixedParser(this)
  }

  class PostfixedParser[E, T] (p1: => Parser[E, T], p2: => Parser[E, _]) extends Parser[E, T] {
    lazy val parser: Parser[E, T] = p1
    lazy val postfix: Parser[E, _] = p2
    def acceptVisitor [R[_, +_]] (visitor: ParserImpl.ParserVisitor[R]): R[E, T] = visitor.visitPostfixedParser(this)
  }

  class LexicalParser[E, T] (l: => Lexer[T]) extends Parser[E, T] {
    lazy val lexer: Lexer[T] = l
    def acceptVisitor [R[_, +_]] (visitor: ParserImpl.ParserVisitor[R]): R[E, T] = visitor.visitLexicalParser(this)
  }

  class TokensParser[E] (val tokens: Seq[E]) extends Parser[E, Seq[E]] {
    def acceptVisitor [R[_, +_]] (visitor: ParserImpl.ParserVisitor[R]): R[E, Seq[E]] = visitor.visitTokensParser(this)
  }
}
