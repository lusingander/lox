package jlox

import org.scalatest.funsuite.AnyFunSuite

class ScannerTest extends AnyFunSuite:

  test("empty"):
    val source = ""
    val sut = new Scanner(source)
    val expected = Seq(
      Token(TokenType.Eof, "", None, 1),
    )
    val actual = sut.scanTokens()
    assert(actual == expected)

  test("single line"):
    val source = """var xyz = "123"; var _x = 456.789;"""
    val sut = new Scanner(source)
    val expected = Seq(
      Token(TokenType.Var, "var", None, 1),
      Token(TokenType.Identifier, "xyz", None, 1),
      Token(TokenType.Equal, "=", None, 1),
      Token(TokenType.String, "\"123\"", Some("123"), 1),
      Token(TokenType.Semicolon, ";", None, 1),
      Token(TokenType.Var, "var", None, 1),
      Token(TokenType.Identifier, "_x", None, 1),
      Token(TokenType.Equal, "=", None, 1),
      Token(TokenType.Number, "456.789", Some(456.789), 1),
      Token(TokenType.Semicolon, ";", None, 1),
      Token(TokenType.Eof, "", None, 1),
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
      Token(TokenType.If, "if", None, 2),
      Token(TokenType.LeftParen, "(", None, 2),
      Token(TokenType.Identifier, "foo", None, 2),
      Token(TokenType.LessEqual, "<=", None, 2),
      Token(TokenType.Identifier, "bar", None, 2),
      Token(TokenType.And, "and", None, 2),
      Token(TokenType.Bang, "!", None, 2),
      Token(TokenType.Identifier, "baz", None, 2),
      Token(TokenType.RightParen, ")", None, 2),
      Token(TokenType.LeftBrace, "{", None, 2),
      Token(TokenType.Return, "return", None, 3),
      Token(TokenType.False, "false", None, 3),
      Token(TokenType.Semicolon, ";", None, 3),
      Token(TokenType.RightBrace, "}", None, 4),
      Token(TokenType.Eof, "", None, 5),
    )
    val actual = sut.scanTokens()
    assert(actual == expected)
