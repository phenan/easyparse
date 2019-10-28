package com.phenan.easyparse.errors

case class ParseError (message: String) extends RuntimeException(message)
