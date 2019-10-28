package com.phenan.easyparse.macroimpl

import scala.reflect.macros.whitebox

class LexerMacros (val c: whitebox.Context) extends MacroCommons {
  import c.universe._

  def lexerMacroImpl (pf: Tree): Tree = pf match {
    case q"{ case ..$cases }" =>
      val info = cases.map(checkLexerCase)
      val lexer = buildComposedLexer(info)

      resetAllSymbols(lexer)
      c.untypecheck(lexer)

    case _ =>
      pf
  }

  private case class PatternInfo (lexer: Tree, pattern: Tree)
  private case class LexerInfo (parts: List[Tree], lexerPatterns: List[PatternInfo], condition: Tree, expression: Tree)

  private def buildComposedLexer(infoList: List[LexerInfo]): Tree = {
    val lexers = infoList.map(buildLexer)
    if (lexers.size == 1) {
      lexers.head
    } else {
      q"Lexer.choice(List(..$lexers))"
    }
  }

  private def buildLexer (info: LexerInfo): Tree = info.lexerPatterns match {
    case Nil =>
      q"Lexer.string(${info.parts.head})"
    case lexerPatterns =>
      val lexers = lexerPatterns.map(pattern => pattern.lexer)

      val prefixedLexers = info.parts.init.zip(lexers).map {
        case (Literal(Constant("")), lexer) =>
          lexer
        case (part, lexer) =>
          q"Lexer.prefixed(Lexer.string($part), $lexer)"
      }

      val sequenceWithoutLast = prefixedLexers.init.foldRight(prefixedLexers.last) { (prefixedLexer, seq) =>
        q"Lexer.seq($prefixedLexer, $seq)"
      }

      val seq = info.parts.last match {
        case Literal(Constant("")) =>
          sequenceWithoutLast
        case postfix =>
          q"Lexer.postfixed($sequenceWithoutLast, Lexer.string($postfix))"
      }

      val patterns = lexerPatterns.map(pattern => pattern.pattern)

      val pat = patterns.init.foldRight(patterns.last) { (pattern, seq) =>
        pq"($pattern, $seq)"
      }

      if (info.condition.nonEmpty) {
        q"$seq.collect { case $pat if ${info.condition} => ${info.expression} }"
      } else {
        q"$seq.map { case $pat => ${info.expression} }"
      }
  }


  private def checkLexerCase (tree: Tree): LexerInfo = tree match {
    case cq"${UnApply(pq"${q"$implicitsObject.StringContextExtension($stringContext.apply(..$parts)).l"}.unapplySeq(..$_)", arguments)} if $cond => $expr"
      if implicitsObject.tpe <:< typeOf[com.phenan.easyparse.Implicits] && stringContext.tpe <:< typeOf[StringContext.type] =>
      val lexerPatterns = arguments.map(checkLexerExtractor)
      LexerInfo(parts, lexerPatterns, cond, expr)
    case _ =>
      c.abort(tree.pos, s"""unsupported pattern.""")
  }

  private def checkLexerExtractor(tree: Tree): PatternInfo = tree match {
    case UnApply(pq"$lexer.unapply(..$_)", arguments) if lexer.tpe <:< typeOf[com.phenan.easyparse.Lexer[_]] && arguments.size == 1 =>
      PatternInfo(lexer, arguments.head)
    case _ =>
      c.abort(tree.pos, s"""unsupported pattern.""")
  }
}
