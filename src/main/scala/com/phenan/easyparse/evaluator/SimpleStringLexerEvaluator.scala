package com.phenan.easyparse.evaluator

import com.phenan.easyparse.{Lexer, LexerImpl}
import com.phenan.easyparse.errors.LexicalError

object SimpleStringLexerEvaluator {

  def runLexer [T] (lex: Lexer[T], in: String): Either[LexicalError, T] = {
    evaluatorVisitor.visit(lex)(in) match {
      case Right((rest, t)) if rest.isEmpty =>
        Right(t)
      case Right((rest, _)) =>
        Left(LexicalError(s"found extra string: $rest"))
      case Left(err) =>
        Left(err)
    }
  }

  private type SimpleLexerEvaluator[+T] = String => Either[LexicalError, (String, T)]

  private val evaluatorVisitor = new LexerImpl.LexerVisitor[SimpleLexerEvaluator] {
    override def visitPureLexer[T] (lexer: LexerImpl.PureLexer[T]): SimpleLexerEvaluator[T] = { in =>
      Right((in, lexer.value))
    }
    override def visitChoiceLexer[T] (lexer: LexerImpl.ChoiceLexer[T]): SimpleLexerEvaluator[T] = { in =>
      def run (evaluators: Seq[SimpleLexerEvaluator[T]]): Either[LexicalError, (String, T)] = evaluators match {
        case head :: tail => head(in) match {
          case Left(_) => run(tail)
          case right   => right
        }
        case Nil => Left(LexicalError("not matched"))
      }
      run(lexer.lexers.map(visit))
    }

    override def visitFilteredLexer[T] (lexer: LexerImpl.FilteredLexer[T]): SimpleLexerEvaluator[T] = { in =>
      visit(lexer.lexer)(in).filterOrElse({ case (_, t) => lexer.filter(t) }, LexicalError("rejected by filter"))
    }

    override def visitMappedLexer[T, U] (lexer: LexerImpl.MappedLexer[T, U]): SimpleLexerEvaluator[U] = { in =>
      visit(lexer.lexer)(in).map { case (rest, t) => (rest, lexer.f(t)) }
    }

    override def visitPartialMappedLexer[T, U] (lexer: LexerImpl.PartialMappedLexer[T, U]): SimpleLexerEvaluator[U] = { in =>
      visit(lexer.lexer)(in).flatMap { case (rest, t) =>
        lexer.f.lift(t) match {
          case Some(u) => Right((rest, u))
          case None    => Left(LexicalError("rejected by filtering"))
        }
      }
    }

    override def visitSequentialLexer[T, U] (lexer: LexerImpl.SequentialLexer[T, U]): SimpleLexerEvaluator[(T, U)] = { in1 =>
      visit(lexer.lexer1)(in1).flatMap { case (in2, t) =>
        visit(lexer.lexer2)(in2).map { case (in3, u) =>
          (in3, (t, u))
        }
      }
    }

    override def visitPrefixedLexer[T] (lexer: LexerImpl.PrefixedLexer[T]): SimpleLexerEvaluator[T] = { in1 =>
      visit(lexer.prefix)(in1).flatMap { case (in2, _) =>
        visit(lexer.lexer)(in2)
      }
    }

    override def visitPostfixedLexer[T] (lexer: LexerImpl.PostfixedLexer[T]): SimpleLexerEvaluator[T] = { in1 =>
      visit(lexer.lexer)(in1).flatMap { case (in2, t) =>
        visit(lexer.postfix)(in2).map { case (in3, _) =>
          (in3, t)
        }
      }
    }

    override def visitRepeatedLexer[T] (lexer: LexerImpl.RepeatedLexer[T]): SimpleLexerEvaluator[Seq[T]] = { in =>
      def run (evaluator: SimpleLexerEvaluator[T], in: String, result: Seq[T]): (String, Seq[T]) = evaluator(in) match {
        case Right((rest, t)) => run(evaluator, rest, result :+ t)
        case Left(_)          => (in, result)
      }
      val result = run(visit(lexer.lexer), in, Seq.empty)
      if (result._2.size >= lexer.minimumRepeat) {
        Right(result)
      } else {
        Left(LexicalError("repeat count is less than minimum repeat"))
      }
    }

    override def visitStringLexer (lexer: LexerImpl.StringLexer): SimpleLexerEvaluator[String] = { in =>
      if (in.startsWith(lexer.string)) Right((in.stripPrefix(lexer.string), lexer.string))
      else Left(LexicalError(s"expected ${lexer.string} but not found"))
    }

    override def visitAnyCharLexer: SimpleLexerEvaluator[Char] = { in =>
      if (in.nonEmpty) Right((in.tail, in.head))
      else Left(LexicalError("empty string"))
    }
  }
}
