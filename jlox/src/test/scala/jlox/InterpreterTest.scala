package jlox

import org.scalatest.funsuite.AnyFunSuite

class InterpreterTest extends AnyFunSuite:

  test("Number +, *"):
    val sut = Interpreter()
    val expr = Expr.Binary(
      left = Expr.Grouping(
        expression = Expr.Binary(
          left = Expr.Literal(LoxDataType.Number(1)),
          operator = Token(TokenType.Plus, "+", LoxDataType.Nil, 1),
          right = Expr.Literal(LoxDataType.Number(2)),
        ),
      ),
      operator = Token(TokenType.Star, "*", LoxDataType.Nil, 1),
      right = Expr.Literal(LoxDataType.Number(3)),
    )
    val exptected = LoxDataType.Number(9)
    val actual = sut.interpret(expr)
    assert(actual == exptected)

  test("String +"):
    val sut = Interpreter()
    val expr = Expr.Binary(
      left = Expr.Literal(LoxDataType.String("foo")),
      operator = Token(TokenType.Plus, "+", LoxDataType.Nil, 1),
      right = Expr.Literal(LoxDataType.String("bar")),
    )
    val exptected = LoxDataType.String("foobar")
    val actual = sut.interpret(expr)
    assert(actual == exptected)

  test("Number <"):
    val sut = Interpreter()
    val expr = Expr.Binary(
      left = Expr.Literal(LoxDataType.Number(5)),
      operator = Token(TokenType.Less, "<", LoxDataType.Nil, 1),
      right = Expr.Literal(LoxDataType.Number(3)),
    )
    val exptected = LoxDataType.Bool(false)
    val actual = sut.interpret(expr)
    assert(actual == exptected)
