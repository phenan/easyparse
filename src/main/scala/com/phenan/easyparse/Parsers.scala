package com.phenan.easyparse

import com.phenan.easyparse.backend.SimpleStringLexerEvaluator

import scala.language.experimental.macros
import com.phenan.easyparse.macroimpl.ParserMacros

trait Parsers extends Implicits {

  type Token

  def lexer: Lexer[Token]
  def whitespace: Lexer[Any]

  type Parser[T] = com.phenan.easyparse.Parser[Token, T]

  object Parser {
    def apply [T] (pf: PartialFunction[String, T]): Parser[T] = macro ParserMacros.parserMacroImpl

    def pure [T] (value: T): Parser[T] = new ParserImpl.PureParser(value)
    def choice [T] (parsers: => Seq[Parser[T]]) = new ParserImpl.ChoiceParser(parsers)

    def seq [T, U] (parser1: => Parser[T], parser2: => Parser[U]): Parser[(T, U)] = new ParserImpl.SequentialParser(parser1, parser2)
    def prefixed [T] (prefix: => Parser[_], parser: => Parser[T]): Parser[T] = new ParserImpl.PrefixedParser(prefix, parser)
    def postfixed [T] (parser: => Parser[T], postfix: => Parser[_]): Parser[T] = new ParserImpl.PostfixedParser(parser, postfix)

    private val lexerEvaluator: SimpleStringLexerEvaluator[Token] = new SimpleStringLexerEvaluator[Token](lexer, whitespace)

    def tokens (string: String): Parser[Seq[Token]] = lexerEvaluator.runLexer(string) match {
      case Right(tokens) => new ParserImpl.TokensParser(tokens)
      case Left(err)     => throw new RuntimeException(s"syntax error in $string. error: $err")
    }

    def fromLexer [T] (lexer: => Lexer[T]): Parser[T] = new ParserImpl.LexicalParser(lexer)
  }

  implicit class ParserOps [T] (parser: => Parser[T]) {
    def map [U] (f: T => U): Parser[U] = new ParserImpl.MappedParser(parser, f)
    def collect [U] (f: PartialFunction[T, U]): Parser[U] = new ParserImpl.PartialMappedParser(parser, f)
    def filter (f: T => Boolean): Parser[T] = new ParserImpl.FilteredParser(parser, f)
  }

}
