package com.phenan.easyparse.macroimpl

import scala.reflect.macros.whitebox

trait MacroCommons {
  val c: whitebox.Context

  import c.universe._

  def resetAllSymbols(tree: Tree): Unit = {
    val resetTreeTraverser: Traverser = new Traverser {
      val stringBuilder = new StringBuilder

      override def traverse (tree: Tree): Unit = {
        if (tree.symbol != null) {
          c.internal.setSymbol(tree, NoSymbol)
        }
        super.traverse(tree)
      }
    }
    resetTreeTraverser.traverse(tree)
  }
}
