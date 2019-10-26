package com.phenan.easyparse

import scala.language.existentials

import org.scalatest._

class LexerBuildTest extends FlatSpec with DiagrammedAssertions with Implicits {
  "lexer" should "be able to build with easy notation" in {
    val any = Lexer.any

    val digit: Lexer[Char] = Lexer {
      case l"${any(n)}" if n.isDigit => n
    }

    assert(digit.isInstanceOf[LexerImpl.PartialMappedLexer[_, _]])
    assert(digit.asInstanceOf[LexerImpl.PartialMappedLexer[_, _]].lexer == LexerImpl.AnyCharLexer)
  }
}
