package com.phenan.easyparse.macroimpl

import scala.reflect.macros.whitebox

class ParserMacros (val c: whitebox.Context) extends MacroCommons {
  import c.universe._

  def parserMacroImpl (pf: Tree): Tree = pf match {
    case q"{ case ..$cases }" =>
      val info = cases.map(checkParserCase)
      val parser = buildComposedParser(info)

      println(parser)

      resetAllSymbols(parser)
      c.untypecheck(parser)

    case _ =>
      pf
  }

  private sealed trait PatternInfo {
    def pattern: Tree
  }

  private case class LexerPatternInfo (lexer: Tree, pattern: Tree) extends PatternInfo
  private case class ParserPatternInfo (parser: Tree, pattern: Tree) extends PatternInfo

  private case class ParserInfo (parts: List[Tree], parserPatterns: List[PatternInfo], condition: Tree, expression: Tree)

  private def buildComposedParser(infoList: List[ParserInfo]): Tree = {
    val parsers = infoList.map(buildParser)
    if (parsers.size == 1) {
      parsers.head
    } else {
      q"Parser.choice(List(..$parsers))"
    }
  }

  private def buildParser (info: ParserInfo): Tree = info.parserPatterns match {
    case Nil =>
      q"Parser.tokens(${info.parts.head})"
    case parserPatterns =>
      val parsers = parserPatterns.map {
        case LexerPatternInfo(lexer, _)   => q"Parser.fromLexer($lexer)"
        case ParserPatternInfo(parser, _) => parser
      }

      val prefixedParsers = info.parts.init.zip(parsers).map {
        case (Literal(Constant("")), parser) =>
          parser
        case (part, parser) =>
          q"Parser.prefixed(Parser.tokens($part), $parser)"
      }

      val sequenceWithoutLast = prefixedParsers.init.foldRight(prefixedParsers.last) { (prefixedParser, seq) =>
        q"Parser.seq($prefixedParser, $seq)"
      }

      val seq = info.parts.last match {
        case Literal(Constant("")) =>
          sequenceWithoutLast
        case postfix =>
          q"Parser.postfixed($sequenceWithoutLast, Parser.tokens($postfix))"
      }

      val patterns = parserPatterns.map(pattern => pattern.pattern)

      val pat = patterns.init.foldRight(patterns.last) { (pattern, seq) =>
        pq"($pattern, $seq)"
      }

      if (info.condition.nonEmpty) {
        q"$seq.collect { case $pat if ${info.condition} => ${info.expression} }"
      } else {
        q"$seq.map { case $pat => ${info.expression} }"
      }
  }

  private def checkParserCase (tree: Tree): ParserInfo = tree match {
    case cq"${UnApply(pq"${q"$implicitsObject.StringContextExtension($stringContext.apply(..$parts)).p"}.unapplySeq(..$_)", arguments)} if $cond => $expr"
      if implicitsObject.tpe <:< typeOf[com.phenan.easyparse.Implicits] && stringContext.tpe <:< typeOf[StringContext.type] =>
      val parserPatterns = arguments.map(checkParserExtractor)
      ParserInfo(parts, parserPatterns, cond, expr)
    case _ =>
      c.abort(tree.pos, s"""unsupported pattern.""")
  }

  private def checkParserExtractor(tree: Tree): PatternInfo = tree match {
    case UnApply(pq"$lexer.unapply(..$_)", arguments) if lexer.tpe <:< typeOf[com.phenan.easyparse.Lexer[_]] && arguments.size == 1 =>
      LexerPatternInfo(lexer, arguments.head)
    case UnApply(pq"$parser.unapply(..$_)", arguments) if parser.tpe <:< typeOf[com.phenan.easyparse.Parser[_, _]] && arguments.size == 1 =>
      ParserPatternInfo(parser, arguments.head)
    case _ =>
      c.abort(tree.pos, s"""unsupported pattern.""")
  }
}
