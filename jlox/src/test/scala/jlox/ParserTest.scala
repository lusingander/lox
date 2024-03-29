package jlox

class ParserTest extends LoxTestBase:

  test("term, factor の優先度"):
    val tokens = Seq(
      token(TokenType.Number, "1", LoxDataType.Number(1)),
      token(TokenType.Star, "*"),
      token(TokenType.Number, "2", LoxDataType.Number(2)),
      token(TokenType.Plus, "+"),
      token(TokenType.Number, "3", LoxDataType.Number(3)),
      token(TokenType.Star, "*"),
      token(TokenType.Number, "4", LoxDataType.Number(4)),
      token(TokenType.Semicolon, ";"),
      token(TokenType.Eof, ""),
    )
    val sut = Parser(tokens)
    val expected = Seq(
      Stmt.Expression(
        expression = Expr.Binary(
          left = Expr.Binary(
            left = Expr.Literal(LoxDataType.Number(1)),
            operator = token(TokenType.Star, "*"),
            right = Expr.Literal(LoxDataType.Number(2)),
          ),
          operator = token(TokenType.Plus, "+"),
          right = Expr.Binary(
            left = Expr.Literal(LoxDataType.Number(3)),
            operator = token(TokenType.Star, "*"),
            right = Expr.Literal(LoxDataType.Number(4)),
          ),
        ),
      ),
    )
    val actual = sut.parse()
    assert(actual == expected)

  test("左結合"):
    val tokens = Seq(
      token(TokenType.Number, "40", LoxDataType.Number(40)),
      token(TokenType.Minus, "-"),
      token(TokenType.Number, "30", LoxDataType.Number(30)),
      token(TokenType.Minus, "-"),
      token(TokenType.Number, "20", LoxDataType.Number(20)),
      token(TokenType.Minus, "-"),
      token(TokenType.Number, "10", LoxDataType.Number(10)),
      token(TokenType.Semicolon, ";"),
      token(TokenType.Eof, ""),
    )
    val sut = Parser(tokens)
    val expected = Seq(
      Stmt.Expression(
        expression = Expr.Binary(
          left = Expr.Binary(
            left = Expr.Binary(
              left = Expr.Literal(LoxDataType.Number(40)),
              operator = token(TokenType.Minus, "-"),
              right = Expr.Literal(LoxDataType.Number(30)),
            ),
            operator = token(TokenType.Minus, "-"),
            right = Expr.Literal(LoxDataType.Number(20)),
          ),
          operator = token(TokenType.Minus, "-"),
          right = Expr.Literal(LoxDataType.Number(10)),
        ),
      ),
    )
    val actual = sut.parse()
    assert(actual == expected)

  test("unary, equality の優先度"):
    val tokens = Seq(
      token(TokenType.Bang, "!"),
      token(TokenType.True, "true", LoxDataType.Bool(true)),
      token(TokenType.EqualEqual, "=="),
      token(TokenType.Bang, "!"),
      token(TokenType.Bang, "!"),
      token(TokenType.False, "false", LoxDataType.Bool(false)),
      token(TokenType.Semicolon, ";"),
      token(TokenType.Eof, ""),
    )
    val sut = Parser(tokens)
    val expected = Seq(
      Stmt.Expression(
        expression = Expr.Binary(
          left = Expr
            .Unary(
              operator = token(TokenType.Bang, "!"),
              right = Expr.Literal(LoxDataType.Bool(true)),
            ),
          operator = token(TokenType.EqualEqual, "=="),
          right = Expr
            .Unary(
              operator = token(TokenType.Bang, "!"),
              right = Expr.Unary(
                operator = token(TokenType.Bang, "!"),
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
      token(TokenType.Number, "10", LoxDataType.Number(10)),
      token(TokenType.Semicolon, ";"),
      token(TokenType.Print, "print"),
      token(TokenType.String, "\"foo\"", LoxDataType.String("foo")),
      token(TokenType.Semicolon, ";"),
      token(TokenType.Eof, ""),
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

  test("for"):
    val tokens = Seq(
      token(TokenType.For, "for"),
      token(TokenType.LeftParen, "("),
      token(TokenType.Var, "var"),
      token(TokenType.Identifier, "i"),
      token(TokenType.Equal, "="),
      token(TokenType.Number, "1", LoxDataType.Number(1)),
      token(TokenType.Semicolon, ";"),
      token(TokenType.Identifier, "i"),
      token(TokenType.Less, "<"),
      token(TokenType.Number, "5", LoxDataType.Number(5)),
      token(TokenType.Semicolon, ";"),
      token(TokenType.Identifier, "i"),
      token(TokenType.Equal, "="),
      token(TokenType.Identifier, "i"),
      token(TokenType.Plus, "+"),
      token(TokenType.Number, "1", LoxDataType.Number(1)),
      token(TokenType.RightParen, ")"),
      token(TokenType.LeftBrace, "{"),
      token(TokenType.Print, "print"),
      token(TokenType.Identifier, "i"),
      token(TokenType.Semicolon, ";"),
      token(TokenType.RightBrace, "}"),
      token(TokenType.Eof, ""),
    )
    val sut = Parser(tokens)
    val expected = Seq(
      Stmt.Block(
        statements = Seq(
          Stmt.Var(
            name = token(TokenType.Identifier, "i"),
            initializer = Some(Expr.Literal(LoxDataType.Number(1))),
          ),
          Stmt.While(
            condition = Expr.Binary(
              left = Expr.Variable(token(TokenType.Identifier, "i")),
              operator = token(TokenType.Less, "<"),
              right = Expr.Literal(LoxDataType.Number(5)),
            ),
            body = Stmt.Block(
              statements = Seq(
                Stmt.Block(
                  statements = Seq(
                    Stmt.Print(
                      expression = Expr.Variable(token(TokenType.Identifier, "i")),
                    ),
                  ),
                ),
                Stmt.Expression(
                  expression = Expr.Assign(
                    name = token(TokenType.Identifier, "i"),
                    value = Expr.Binary(
                      left = Expr.Variable(token(TokenType.Identifier, "i")),
                      operator = token(TokenType.Plus, "+"),
                      right = Expr.Literal(LoxDataType.Number(1)),
                    ),
                  ),
                ),
              ),
            ),
          ),
        ),
      ),
    )
    val actual = sut.parse()
    assert(actual == expected)
