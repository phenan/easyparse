package com.phenan.easyparse.evaluator

import com.phenan.easyparse.errors.ParseError
import com.phenan.easyparse.{Lexer, LexerImpl, ParserImpl}

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.combinator.token.Tokens
import scala.util.parsing.input.{CharSequenceReader, Reader}

class ScalaParserBackedEvaluator [E] (lexer: Lexer[E], whitespaceLexer: Lexer[Any]) {
  def runParse [T] (in: String, parser: com.phenan.easyparse.Parser[E, T]): Either[ParseError, T] = {
    parsers.runParse(in, parser)
  }

  private val parsers = new ScalaParserBackedEvaluator.ScalaParsersBackedParser(lexer, whitespaceLexer)
}

object ScalaParserBackedEvaluator {
  trait EvaluatorTokens extends Tokens {
    case class EvaluatorToken[+E] (value: E, chars: String) extends Token {
      def map [F] (f: E => F): EvaluatorToken[F] = EvaluatorToken(f(value), chars)
      def ~ [F] (f: EvaluatorToken[F]): EvaluatorToken[(E, F)] = EvaluatorToken((value, f.value), chars + f.chars)
    }
  }

  class ScalaParsersBackedParser [E] (lexer: Lexer[E], whitespaceLexer: Lexer[Any]) extends Parsers {
    override type Elem = ScalaLexicalBackedLexer.Token

    def runParse [T] (in: String, parser: com.phenan.easyparse.Parser[E, T]): Either[ParseError, T] = {
      parserVisitor.visit(parser)(new ScalaLexicalBackedLexer.Scanner(in)) match {
        case Success(result, rest) if rest.atEnd => Right(result)
        case Success(_, rest)  => Left(ParseError(s"extra input: $rest"))
        case NoSuccess(msg, _) => Left(ParseError(msg))
      }
    }

    private val parserVisitor = new ParserImpl.ParserVisitor[Lambda[(L, +[T]) => Parser[T]]] {
      override def visitPureParser[L, T] (parser: ParserImpl.PureParser[L, T]): Parser[T] = {
        success(parser.value)
      }

      override def visitChoiceParser[L, T] (parser: ParserImpl.ChoiceParser[L, T]): Parser[T] = {
        parser.parsers.map(visit).foldRight[Parser[T]](failure("no more choice"))(_ | _)
      }

      override def visitFilteredParser[L, T] (parser: ParserImpl.FilteredParser[L, T]): Parser[T] = {
        visit(parser.parser).filter(parser.filter)
      }

      override def visitMappedParser[L, T1, T2] (parser: ParserImpl.MappedParser[L, T1, T2]): Parser[T2] = {
        visit(parser.parser) ^^ parser.f
      }

      override def visitPartialMappedParser[L, T1, T2] (parser: ParserImpl.PartialMappedParser[L, T1, T2]): Parser[T2] = {
        visit(parser.parser) ^? parser.f
      }

      override def visitSequentialParser[L, T1, T2] (parser: ParserImpl.SequentialParser[L, T1, T2]): Parser[(T1, T2)] = {
        visit(parser.parser1) ~ visit(parser.parser2) ^^ { case t1 ~ t2 => (t1, t2) }
      }

      override def visitPrefixedParser[L, T] (parser: ParserImpl.PrefixedParser[L, T]): Parser[T] = {
        visit(parser.prefix) ~> visit(parser.parser)
      }

      override def visitPostfixedParser[L, T] (parser: ParserImpl.PostfixedParser[L, T]): Parser[T] = {
        visit(parser.parser) <~ visit(parser.postfix)
      }

      override def visitLexicalParser[L, T] (parser: ParserImpl.LexicalParser[L, T]): Parser[T] = {
        accept("lexical", new PartialFunction[Elem, T] {
          val reparseElem: Elem => Option[T] = {
            case ScalaLexicalBackedLexer.EvaluatorToken(e, chars) =>
              ScalaLexicalBackedLexer.LexerVisitor.visit(parser.lexer)(new CharSequenceReader(chars)) match {
                case ScalaLexicalBackedLexer.Success(ScalaLexicalBackedLexer.EvaluatorToken(e2, chars2), rest) if e == e2 && chars == chars2 && rest.atEnd => Some(e2)
                case _ => None
              }
            case _ => None
          }
          override def isDefinedAt (x: Elem): Boolean = reparseElem(x).nonEmpty
          override def apply (v1: Elem): T = reparseElem(v1).get
        })
      }

      override def visitTokensParser[L] (parser: ParserImpl.TokensParser[L]): Parser[Seq[L]] = {
        parser.tokens
          .map(token => accept(s"token $token", { case ScalaLexicalBackedLexer.EvaluatorToken(e, _) if e == token => token }))
          .foldRight(success(Seq.empty[L])) { (parser, result) =>
            parser ~ result ^^ { case e ~ es => e +: es }
          }
      }
    }

    object ScalaLexicalBackedLexer extends Lexical with EvaluatorTokens {
      override def token: Parser[Token] = LexerVisitor.visit(lexer)
      override def whitespace: Parser[Any] = LexerVisitor.visit(whitespaceLexer.*)

      object LexerVisitor extends LexerImpl.LexerVisitor[Lambda[+[E] => Parser[EvaluatorToken[E]]]] {
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
}
