package jlox

import org.scalatest.funsuite.AnyFunSuite

class ParserTest extends AnyFunSuite:

  test("term, factor の優先度"):
    val tokens = Seq(
      Token(TokenType.Number, "1", Some(1), 1),
      Token(TokenType.Star, "*", None, 1),
      Token(TokenType.Number, "2", Some(2), 1),
      Token(TokenType.Plus, "+", None, 1),
      Token(TokenType.Number, "3", Some(3), 1),
      Token(TokenType.Star, "*", None, 1),
      Token(TokenType.Number, "4", Some(4), 1),
      Token(TokenType.Eof, "", None, 1),
    )
    val sut = Parser(tokens)
    val expected = Expr.Binary(
      left = Expr.Binary(
        left = Expr.Literal(Some(1)),
        operator = Token(TokenType.Star, "*", None, 1),
        right = Expr.Literal(Some(2)),
      ),
      operator = Token(TokenType.Plus, "+", None, 1),
      right = Expr.Binary(
        left = Expr.Literal(Some(3)),
        operator = Token(TokenType.Star, "*", None, 1),
        right = Expr.Literal(Some(4)),
      ),
    )
    val actual = sut.parse()
    assert(actual == expected)

  test("左結合"):
    val tokens = Seq(
      Token(TokenType.Number, "40", Some(40), 1),
      Token(TokenType.Minus, "-", None, 1),
      Token(TokenType.Number, "30", Some(30), 1),
      Token(TokenType.Minus, "-", None, 1),
      Token(TokenType.Number, "20", Some(20), 1),
      Token(TokenType.Minus, "-", None, 1),
      Token(TokenType.Number, "10", Some(10), 1),
      Token(TokenType.Eof, "", None, 1),
    )
    val sut = Parser(tokens)
    val expected = Expr.Binary(
      left = Expr.Binary(
        left = Expr.Binary(
          left = Expr.Literal(Some(40)),
          operator = Token(TokenType.Minus, "-", None, 1),
          right = Expr.Literal(Some(30)),
        ),
        operator = Token(TokenType.Minus, "-", None, 1),
        right = Expr.Literal(Some(20)),
      ),
      operator = Token(TokenType.Minus, "-", None, 1),
      right = Expr.Literal(Some(10)),
    )
    val actual = sut.parse()
    assert(actual == expected)

  test("unary, equality の優先度"):
    val tokens = Seq(
      Token(TokenType.Bang, "!", None, 1),
      Token(TokenType.True, "true", Some(true), 1),
      Token(TokenType.EqualEqual, "==", None, 1),
      Token(TokenType.Bang, "!", None, 1),
      Token(TokenType.Bang, "!", None, 1),
      Token(TokenType.False, "false", Some(false), 1),
      Token(TokenType.Eof, "", None, 1),
    )
    val sut = Parser(tokens)
    val expected = Expr.Binary(
      left = Expr
        .Unary(
          operator = Token(TokenType.Bang, "!", None, 1),
          right = Expr.Literal(Some(true)),
        ),
      operator = Token(TokenType.EqualEqual, "==", None, 1),
      right = Expr
        .Unary(
          operator = Token(TokenType.Bang, "!", None, 1),
          right = Expr.Unary(
            operator = Token(TokenType.Bang, "!", None, 1),
            right = Expr.Literal(Some(false)),
          ),
        ),
    )
    val actual = sut.parse()
    assert(actual == expected)
