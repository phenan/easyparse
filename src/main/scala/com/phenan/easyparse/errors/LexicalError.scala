package com.phenan.easyparse.errors

case class LexicalError (message: String) extends RuntimeException(message)
