package jlox

import java.io.ByteArrayOutputStream
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

trait LoxTestBase extends AnyFunSuite with TableDrivenPropertyChecks:

  protected def token(
      tp: TokenType,
      lexeme: String,
      literal: LoxDataType = LoxDataType.Nil,
      line: Int = 1,
  ): Token = Token(tp, lexeme, literal, line)

  protected def assertStdout(execute: => Unit)(expected: String): Unit =
    val out = ByteArrayOutputStream()
    Console.withOut(out):
      execute
    assert(out.toString() == expected)
