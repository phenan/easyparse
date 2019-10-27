package com.phenan.easyparse.evaluator

import com.phenan.easyparse.{Lexer, LexerImpl, ParserImpl}

import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.combinator.token.Tokens

class ScalaParserBackedEvaluator {
  
  trait EvaluatorTokens extends Tokens {
    case class EvaluatorToken[+E] (value: E, chars: String) extends Token {
      def map [F] (f: E => F): EvaluatorToken[F] = EvaluatorToken(f(value), chars)

      def ~ [F] (f: EvaluatorToken[F]): EvaluatorToken[(E, F)] = EvaluatorToken((value, f.value), chars + f.chars)
    }
  }

  class ScalaLexicalBackedLexer [E] (lexer: Lexer[E]) extends Lexical with EvaluatorTokens {
    override def token: Parser[Token] = lexerVisitor.visit(lexer)
    override def whitespace: Parser[Any] = ???

    private val lexerVisitor = new LexerImpl.LexerVisitor[Lambda[+[E] => Parser[EvaluatorToken[E]]]] {
      override def visitPureLexer[T] (lexer: LexerImpl.PureLexer[T]): Parser[EvaluatorToken[T]] = {
        success(lexer.value).map(EvaluatorToken(_, ""))
      }

      override def visitChoiceLexer[T] (lexer: LexerImpl.ChoiceLexer[T]): Parser[EvaluatorToken[T]] = {
        lexer.lexers.map(visit).foldRight[Parser[EvaluatorToken[T]]](failure("no more choice"))(_ | _)
      }

      override def visitFilteredLexer[T] (lexer: LexerImpl.FilteredLexer[T]): Parser[EvaluatorToken[T]] = {
        visit(lexer.lexer).filter(token => lexer.filter(token.value))
      }

      override def visitMappedLexer[T, U] (lexer: LexerImpl.MappedLexer[T, U]): Parser[EvaluatorToken[U]] = {
        visit(lexer.lexer).map(token => token.map(lexer.f))
      }

      override def visitPartialMappedLexer[T, U] (lexer: LexerImpl.PartialMappedLexer[T, U]): Parser[EvaluatorToken[U]] = {
        visit(lexer.lexer) ^? { case token if lexer.f.isDefinedAt(token.value) => token.map(lexer.f) }
      }

      override def visitSequentialLexer[T, U] (lexer: LexerImpl.SequentialLexer[T, U]): Parser[EvaluatorToken[(T, U)]] = {
        visit(lexer.lexer1) ~ visit(lexer.lexer2) ^^ { case t ~ u => t ~ u }
      }

      override def visitPrefixedLexer[T] (lexer: LexerImpl.PrefixedLexer[T]): Parser[EvaluatorToken[T]] = {
        visit(lexer.prefix) ~> visit(lexer.lexer)
      }

      override def visitPostfixedLexer[T] (lexer: LexerImpl.PostfixedLexer[T]): Parser[EvaluatorToken[T]] = {
        visit(lexer.lexer) <~ visit(lexer.postfix)
      }

      override def visitRepeatedLexer[T] (lexer: LexerImpl.RepeatedLexer[T]): Parser[EvaluatorToken[Seq[T]]] = {
        val lex = visit(lexer.lexer)
        val listOfTokensParser = {
          if (lexer.minimumRepeat == 0) lex.*
          else if (lexer.minimumRepeat == 1) lex.+
          else repN(lexer.minimumRepeat, lex) ~ lex.* ^^ { case x ~ y => x ++ y }
        }
        listOfTokensParser.map(tokens => tokens.foldLeft(EvaluatorToken(Seq.empty[T], "")) { (result, token) =>
          EvaluatorToken(result.value :+ token.value, result.chars + token.chars)
        })
      }

      override def visitStringLexer (lexer: LexerImpl.StringLexer): Parser[EvaluatorToken[String]] = {
        acceptSeq(lexer.string).map(_.mkString).map(string => EvaluatorToken(string, string))
      }

      override def visitAnyCharLexer: Parser[EvaluatorToken[Char]] = {
        elem("any character", _ => true).map(char => EvaluatorToken(char, char.toString))
      }
    }
  }
}
