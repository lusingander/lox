package jlox

import org.scalatest.funsuite.AnyFunSuite

class ScannerTest extends AnyFunSuite:

  test("empty"):
    val source = ""
    val sut = Scanner(source)
    val expected = Seq(
      Token(TokenType.Eof, "", LoxDataType.Nil, 1),
    )
    val actual = sut.scanTokens()
    assert(actual == expected)

  test("single line"):
    val source = """var xyz = "123"; var _x = 456.789;"""
    val sut = Scanner(source)
    val expected = Seq(
      Token(TokenType.Var, "var", LoxDataType.Nil, 1),
      Token(TokenType.Identifier, "xyz", LoxDataType.Nil, 1),
      Token(TokenType.Equal, "=", LoxDataType.Nil, 1),
      Token(TokenType.String, "\"123\"", LoxDataType.String("123"), 1),
      Token(TokenType.Semicolon, ";", LoxDataType.Nil, 1),
      Token(TokenType.Var, "var", LoxDataType.Nil, 1),
      Token(TokenType.Identifier, "_x", LoxDataType.Nil, 1),
      Token(TokenType.Equal, "=", LoxDataType.Nil, 1),
      Token(TokenType.Number, "456.789", LoxDataType.Number(456.789), 1),
      Token(TokenType.Semicolon, ";", LoxDataType.Nil, 1),
      Token(TokenType.Eof, "", LoxDataType.Nil, 1),
    )
    val actual = sut.scanTokens()
    assert(actual == expected)

  test("multi lines"):
    val source = """
    |if (foo <= bar and !baz) {
    |  return false;
    |}
    """.stripMargin
    val sut = Scanner(source)
    val expected = Seq(
      Token(TokenType.If, "if", LoxDataType.Nil, 2),
      Token(TokenType.LeftParen, "(", LoxDataType.Nil, 2),
      Token(TokenType.Identifier, "foo", LoxDataType.Nil, 2),
      Token(TokenType.LessEqual, "<=", LoxDataType.Nil, 2),
      Token(TokenType.Identifier, "bar", LoxDataType.Nil, 2),
      Token(TokenType.And, "and", LoxDataType.Nil, 2),
      Token(TokenType.Bang, "!", LoxDataType.Nil, 2),
      Token(TokenType.Identifier, "baz", LoxDataType.Nil, 2),
      Token(TokenType.RightParen, ")", LoxDataType.Nil, 2),
      Token(TokenType.LeftBrace, "{", LoxDataType.Nil, 2),
      Token(TokenType.Return, "return", LoxDataType.Nil, 3),
      Token(TokenType.False, "false", LoxDataType.Nil, 3),
      Token(TokenType.Semicolon, ";", LoxDataType.Nil, 3),
      Token(TokenType.RightBrace, "}", LoxDataType.Nil, 4),
      Token(TokenType.Eof, "", LoxDataType.Nil, 5),
    )
    val actual = sut.scanTokens()
    assert(actual == expected)
