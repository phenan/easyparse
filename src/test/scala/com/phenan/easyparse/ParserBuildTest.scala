package com.phenan.easyparse

import com.phenan.easyparse.backend.ScalaParserCombinatorBackend

import scala.language.existentials
import org.scalatest._

sealed trait MyToken
case class IntToken (x: Int) extends MyToken
case class WordToken (x: String) extends MyToken

case class Foo (i: IntToken)

class ParserBuildTest extends FlatSpec with DiagrammedAssertions with Parsers with ScalaParserCombinatorBackend {
  override type Token = MyToken

  val digit = Lexer.any.filter(_.isDigit)
  val letter = Lexer.any.filter(_.isLetter)

  val intToken = digit.+.map(_.mkString.toInt).map(IntToken)
  val wordToken = letter.+.map(_.mkString).map(WordToken)

  override val lexer: Lexer[Token] = Lexer.choice(Seq(intToken, wordToken))

  override val whitespace: Lexer[Any] = Lexer {
    case l" " => " "
  }

  "simple parser" should "be able to build with easy notation" in {

    val foo: Parser[Foo] = Parser {
      case p"foo ${intToken(n)}" => Foo(n)
    }

    assert(foo.isInstanceOf[ParserImpl.MappedParser[_, _, _]])
    assert(foo.asInstanceOf[ParserImpl.MappedParser[_, _, _]].parser == ParserImpl.PrefixedParser(ParserImpl.TokensParser[Token](Seq(WordToken("foo"))), ParserImpl.LexicalParser[Token, IntToken](intToken)))

    assert(runParse("foo 5", foo) == Right(Foo(IntToken(5))))
  }

}
