package com.phenan.easyparse

import com.phenan.easyparse.backend.ScalaParserCombinatorBackend

import scala.language.existentials
import org.scalatest._

sealed trait MyToken
case class IntToken (x: Int) extends MyToken
case class WordToken (x: String) extends MyToken
case object PlusSymbol extends MyToken

sealed trait Expr
case class IntLit (i: IntToken) extends Expr
case class Add (a: IntToken, b: Expr) extends Expr

class ParserBuildTest extends FlatSpec with DiagrammedAssertions with Parsers with ScalaParserCombinatorBackend {
  override type Token = MyToken

  val digit = Lexer.any.filter(_.isDigit)
  val letter = Lexer.any.filter(_.isLetter)

  lazy val intToken = digit.+.map(_.mkString.toInt).map(IntToken)
  lazy val wordToken = letter.+.map(_.mkString).map(WordToken)
  lazy val plusToken = Lexer.string("+").map(_ => PlusSymbol)

  override lazy val lexer: Lexer[Token] = Lexer.choice(Seq(intToken, wordToken, plusToken))

  override lazy val whitespace: Lexer[Any] = Lexer {
    case l" " => " "
  }

  "simple parser" should "be able to build with easy notation" in {

    lazy val expr: Parser[Expr] = Parser {
      case p"${intToken(n)} + ${expr(e)}" => Add(n, e)
      case p"${intToken(n)}"              => IntLit(n)
    }

    assert(runParse("5", expr) == Right(IntLit(IntToken(5))))
  }

}
