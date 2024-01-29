package jlox

import org.scalatest.funsuite.AnyFunSuite

class ScannerTest extends AnyFunSuite:

  test("empty"):
    val source = ""
    val sut = new Scanner(source)
    val expected = Seq(
      Token(TokenType.Eof, "", null, 1),
    )
    val actual = sut.scanTokens()
    assert(actual == expected)

  test("var"):
    val source = """var xyz = "123"; var _x = 456.789;"""
    val sut = new Scanner(source)
    val expected = Seq(
      Token(TokenType.Var, "var", null, 1),
      Token(TokenType.Identifier, "xyz", null, 1),
      Token(TokenType.Equal, "=", null, 1),
      Token(TokenType.String, "\"123\"", "123", 1),
      Token(TokenType.Semicolon, ";", null, 1),
      Token(TokenType.Var, "var", null, 1),
      Token(TokenType.Identifier, "_x", null, 1),
      Token(TokenType.Equal, "=", null, 1),
      Token(TokenType.Number, "456.789", 456.789, 1),
      Token(TokenType.Semicolon, ";", null, 1),
      Token(TokenType.Eof, "", null, 1),
    )
    val actual = sut.scanTokens()
    assert(actual == expected)

  test("multi lines"):
    val source = """
    |if (foo <= bar and !baz) {
    |  return false;
    |}
    """.stripMargin
    val sut = new Scanner(source)
    val expected = Seq(
      Token(TokenType.If, "if", null, 2),
      Token(TokenType.LeftParen, "(", null, 2),
      Token(TokenType.Identifier, "foo", null, 2),
      Token(TokenType.LessEqual, "<=", null, 2),
      Token(TokenType.Identifier, "bar", null, 2),
      Token(TokenType.And, "and", null, 2),
      Token(TokenType.Bang, "!", null, 2),
      Token(TokenType.Identifier, "baz", null, 2),
      Token(TokenType.RightParen, ")", null, 2),
      Token(TokenType.LeftBrace, "{", null, 2),
      Token(TokenType.Return, "return", null, 3),
      Token(TokenType.False, "false", null, 3),
      Token(TokenType.Semicolon, ";", null, 3),
      Token(TokenType.RightBrace, "}", null, 4),
      Token(TokenType.Eof, "", null, 5),
    )
    val actual = sut.scanTokens()
    assert(actual == expected)
