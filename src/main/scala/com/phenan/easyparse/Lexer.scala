package com.phenan.easyparse

import scala.language.experimental.macros
import com.phenan.easyparse.macroimpl.LexerMacros

sealed trait Lexer[+T] {
  def * : Lexer[Seq[T]] = LexerImpl.RepeatedLexer(this, 0)
  def + : Lexer[Seq[T]] = LexerImpl.RepeatedLexer(this, 1)

  def map [U] (f: T => U): Lexer[U] = LexerImpl.MappedLexer(this, f)
  def collect [U] (f: PartialFunction[T, U]): Lexer[U] = LexerImpl.PartialMappedLexer(this, f)
  def filter (f: T => Boolean): Lexer[T] = LexerImpl.FilteredLexer(this, f)

  // This method is used only for type checking.
  def unapply (arg: String): Option[T] = None

  private[easyparse] def acceptVisitor [R[+_]] (visitor: LexerImpl.LexerVisitor[R]): R[T]
}

object Lexer {
  def apply [T] (pf: PartialFunction[String, T]): Lexer[T] = macro LexerMacros.lexerMacroImpl

  def pure [T] (value: T): Lexer[T] = LexerImpl.PureLexer(value)
  def choice [T] (lexers: Seq[Lexer[T]]) = LexerImpl.ChoiceLexer(lexers)

  def seq [T, U] (lexer1: Lexer[T], lexer2: Lexer[U]): Lexer[(T, U)] = LexerImpl.SequentialLexer(lexer1, lexer2)
  def prefixed [T] (lexer1: Lexer[_], lexer2: Lexer[T]): Lexer[T] = LexerImpl.PrefixedLexer(lexer1, lexer2)
  def postfixed [T] (lexer1: Lexer[T], lexer2: Lexer[_]): Lexer[T] = LexerImpl.PostfixedLexer(lexer1, lexer2)

  def string (str: String): Lexer[String] = LexerImpl.StringLexer(str)

  def any: Lexer[Char] = LexerImpl.AnyCharLexer
}

private[easyparse] object LexerImpl {
  trait LexerVisitor [R[+_]] {
    def visit [T] (lexer: Lexer[T]): R[T] = lexer.acceptVisitor(this)

    def visitPureLexer [T] (lexer: PureLexer[T]): R[T]
    def visitChoiceLexer [T] (lexer: ChoiceLexer[T]): R[T]
    def visitFilteredLexer [T] (lexer: FilteredLexer[T]): R[T]
    def visitMappedLexer [T, U] (lexer: MappedLexer[T, U]): R[U]
    def visitPartialMappedLexer [T, U] (lexer: PartialMappedLexer[T, U]): R[U]
    def visitSequentialLexer [T, U] (lexer: SequentialLexer[T, U]): R[(T, U)]
    def visitPrefixedLexer [T] (lexer: PrefixedLexer[T]): R[T]
    def visitPostfixedLexer [T] (lexer: PostfixedLexer[T]): R[T]
    def visitRepeatedLexer [T] (lexer: RepeatedLexer[T]): R[Seq[T]]
    def visitStringLexer (lexer: StringLexer): R[String]
    def visitAnyCharLexer: R[Char]
  }

  case class PureLexer[T] (value: T) extends Lexer[T] {
    def acceptVisitor [R[+_]] (visitor: LexerImpl.LexerVisitor[R]): R[T] = visitor.visitPureLexer(this)
  }

  case class ChoiceLexer[T] (lexers: Seq[Lexer[T]]) extends Lexer[T] {
    def acceptVisitor [R[+_]] (visitor: LexerImpl.LexerVisitor[R]): R[T] = visitor.visitChoiceLexer(this)
  }

  case class FilteredLexer[T] (lexer: Lexer[T], filter: T => Boolean) extends Lexer[T] {
    def acceptVisitor [R[+_]] (visitor: LexerImpl.LexerVisitor[R]): R[T] = visitor.visitFilteredLexer(this)
  }

  case class MappedLexer[T, U] (lexer: Lexer[T], f: T => U) extends Lexer[U] {
    def acceptVisitor [R[+_]] (visitor: LexerImpl.LexerVisitor[R]): R[U] = visitor.visitMappedLexer(this)
  }

  case class PartialMappedLexer[T, U] (lexer: Lexer[T], f: PartialFunction[T, U]) extends Lexer[U] {
    def acceptVisitor [R[+_]] (visitor: LexerImpl.LexerVisitor[R]): R[U] = visitor.visitPartialMappedLexer(this)
  }

  case class SequentialLexer[T, U] (lexer1: Lexer[T], lexer2: Lexer[U]) extends Lexer[(T, U)] {
    def acceptVisitor [R[+_]] (visitor: LexerImpl.LexerVisitor[R]): R[(T, U)] = visitor.visitSequentialLexer(this)
  }

  case class PrefixedLexer[T] (prefix: Lexer[_], lexer: Lexer[T]) extends Lexer[T] {
    def acceptVisitor [R[+_]] (visitor: LexerImpl.LexerVisitor[R]): R[T] = visitor.visitPrefixedLexer(this)
  }

  case class PostfixedLexer[T] (lexer: Lexer[T], postfix: Lexer[_]) extends Lexer[T] {
    def acceptVisitor [R[+_]] (visitor: LexerImpl.LexerVisitor[R]): R[T] = visitor.visitPostfixedLexer(this)
  }

  case class RepeatedLexer[T] (lexer: Lexer[T], minimumRepeat: Int) extends Lexer[Seq[T]] {
    def acceptVisitor [R[+_]] (visitor: LexerImpl.LexerVisitor[R]): R[Seq[T]] = visitor.visitRepeatedLexer(this)
  }

  case class StringLexer (string: String) extends Lexer[String] {
    def acceptVisitor [R[+_]] (visitor: LexerImpl.LexerVisitor[R]): R[String] = visitor.visitStringLexer(this)
  }

  case object AnyCharLexer extends Lexer[Char] {
    def acceptVisitor [R[+_]] (visitor: LexerImpl.LexerVisitor[R]): R[Char] = visitor.visitAnyCharLexer
  }
}
