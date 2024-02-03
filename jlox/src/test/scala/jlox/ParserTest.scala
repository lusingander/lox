package jlox

import org.scalatest.funsuite.AnyFunSuite

class ParserTest extends AnyFunSuite:

  test("term, factor の優先度"):
    val tokens = Seq(
      Token(TokenType.Number, "1", LoxDataType.Number(1), 1),
      Token(TokenType.Star, "*", LoxDataType.Nil, 1),
      Token(TokenType.Number, "2", LoxDataType.Number(2), 1),
      Token(TokenType.Plus, "+", LoxDataType.Nil, 1),
      Token(TokenType.Number, "3", LoxDataType.Number(3), 1),
      Token(TokenType.Star, "*", LoxDataType.Nil, 1),
      Token(TokenType.Number, "4", LoxDataType.Number(4), 1),
      Token(TokenType.Semicolon, ";", LoxDataType.Nil, 1),
      Token(TokenType.Eof, "", LoxDataType.Nil, 1),
    )
    val sut = Parser(tokens)
    val expected = Seq(
      Stmt.Expression(
        expression = Expr.Binary(
          left = Expr.Binary(
            left = Expr.Literal(LoxDataType.Number(1)),
            operator = Token(TokenType.Star, "*", LoxDataType.Nil, 1),
            right = Expr.Literal(LoxDataType.Number(2)),
          ),
          operator = Token(TokenType.Plus, "+", LoxDataType.Nil, 1),
          right = Expr.Binary(
            left = Expr.Literal(LoxDataType.Number(3)),
            operator = Token(TokenType.Star, "*", LoxDataType.Nil, 1),
            right = Expr.Literal(LoxDataType.Number(4)),
          ),
        ),
      ),
    )
    val actual = sut.parse()
    assert(actual == expected)

  test("左結合"):
    val tokens = Seq(
      Token(TokenType.Number, "40", LoxDataType.Number(40), 1),
      Token(TokenType.Minus, "-", LoxDataType.Nil, 1),
      Token(TokenType.Number, "30", LoxDataType.Number(30), 1),
      Token(TokenType.Minus, "-", LoxDataType.Nil, 1),
      Token(TokenType.Number, "20", LoxDataType.Number(20), 1),
      Token(TokenType.Minus, "-", LoxDataType.Nil, 1),
      Token(TokenType.Number, "10", LoxDataType.Number(10), 1),
      Token(TokenType.Semicolon, ";", LoxDataType.Nil, 1),
      Token(TokenType.Eof, "", LoxDataType.Nil, 1),
    )
    val sut = Parser(tokens)
    val expected = Seq(
      Stmt.Expression(
        expression = Expr.Binary(
          left = Expr.Binary(
            left = Expr.Binary(
              left = Expr.Literal(LoxDataType.Number(40)),
              operator = Token(TokenType.Minus, "-", LoxDataType.Nil, 1),
              right = Expr.Literal(LoxDataType.Number(30)),
            ),
            operator = Token(TokenType.Minus, "-", LoxDataType.Nil, 1),
            right = Expr.Literal(LoxDataType.Number(20)),
          ),
          operator = Token(TokenType.Minus, "-", LoxDataType.Nil, 1),
          right = Expr.Literal(LoxDataType.Number(10)),
        ),
      ),
    )
    val actual = sut.parse()
    assert(actual == expected)

  test("unary, equality の優先度"):
    val tokens = Seq(
      Token(TokenType.Bang, "!", LoxDataType.Nil, 1),
      Token(TokenType.True, "true", LoxDataType.Bool(true), 1),
      Token(TokenType.EqualEqual, "==", LoxDataType.Nil, 1),
      Token(TokenType.Bang, "!", LoxDataType.Nil, 1),
      Token(TokenType.Bang, "!", LoxDataType.Nil, 1),
      Token(TokenType.False, "false", LoxDataType.Bool(false), 1),
      Token(TokenType.Semicolon, ";", LoxDataType.Nil, 1),
      Token(TokenType.Eof, "", LoxDataType.Nil, 1),
    )
    val sut = Parser(tokens)
    val expected = Seq(
      Stmt.Expression(
        expression = Expr.Binary(
          left = Expr
            .Unary(
              operator = Token(TokenType.Bang, "!", LoxDataType.Nil, 1),
              right = Expr.Literal(LoxDataType.Bool(true)),
            ),
          operator = Token(TokenType.EqualEqual, "==", LoxDataType.Nil, 1),
          right = Expr
            .Unary(
              operator = Token(TokenType.Bang, "!", LoxDataType.Nil, 1),
              right = Expr.Unary(
                operator = Token(TokenType.Bang, "!", LoxDataType.Nil, 1),
                right = Expr.Literal(LoxDataType.Bool(false)),
              ),
            ),
        ),
      ),
    )
    val actual = sut.parse()
    assert(actual == expected)

  test("複数の文"):
    val tokens = Seq(
      Token(TokenType.Number, "10", LoxDataType.Number(10), 1),
      Token(TokenType.Semicolon, ";", LoxDataType.Nil, 1),
      Token(TokenType.Print, "print", LoxDataType.Nil, 1),
      Token(TokenType.String, "\"foo\"", LoxDataType.String("foo"), 1),
      Token(TokenType.Semicolon, ";", LoxDataType.Nil, 1),
      Token(TokenType.Eof, "", LoxDataType.Nil, 1),
    )
    val sut = Parser(tokens)
    val expected = Seq(
      Stmt.Expression(
        expression = Expr.Literal(LoxDataType.Number(10)),
      ),
      Stmt.Print(
        expression = Expr.Literal(LoxDataType.String("foo")),
      ),
    )
    val actual = sut.parse()
    assert(actual == expected)
