package jlox

class InterpreterTest extends LoxTestBase:

  test("Number grouping +, *"):
    val sut = Interpreter()
    val expr = Expr.Binary(
      left = Expr.Grouping(
        expression = Expr.Binary(
          left = Expr.Literal(LoxDataType.Number(1)),
          operator = token(TokenType.Plus, "+"),
          right = Expr.Literal(LoxDataType.Number(2)),
        ),
      ),
      operator = token(TokenType.Star, "*"),
      right = Expr.Literal(LoxDataType.Number(3)),
    )
    val stmts = Seq(Stmt.Print(expr))
    Resolver(sut).resolve(stmts)
    val expected = LoxDataType.Number(9)
    assertOutput(sut.interpret(stmts))(expected.toString() + "\n")

  test("Number +, -, *, /"):
    val table = Table(
      ("left", "right", "operator", "expected"),
      (5, 2, (TokenType.Plus, "+"), 7.0),
      (5, 2, (TokenType.Minus, "-"), 3.0),
      (5, 2, (TokenType.Star, "*"), 10.0),
      (5, 2, (TokenType.Slash, "/"), 2.5),
    )
    forAll(table): (left, right, operator, expected) =>
      testSimpleBinaryExpr(
        left = LoxDataType.Number(left),
        right = LoxDataType.Number(right),
        operator = operator,
        expected = LoxDataType.Number(expected),
      )

  test("String +"):
    val table = Table(
      ("left", "right", "operator", "expected"),
      ("foo", "bar", (TokenType.Plus, "+"), "foobar"),
    )
    forAll(table): (left, right, operator, expected) =>
      testSimpleBinaryExpr(
        left = LoxDataType.String(left),
        right = LoxDataType.String(right),
        operator = operator,
        expected = LoxDataType.String(expected),
      )

  test("Number <, <=, >, >="):
    val table = Table(
      ("left", "right", "operator", "expected"),
      (3, 5, (TokenType.Less, "<"), true),
      (3, 3, (TokenType.Less, "<"), false),
      (3, 2, (TokenType.Less, "<"), false),
      (3, 5, (TokenType.LessEqual, "<="), true),
      (3, 3, (TokenType.LessEqual, "<="), true),
      (3, 2, (TokenType.LessEqual, "<="), false),
      (3, 5, (TokenType.Greater, ">"), false),
      (3, 3, (TokenType.Greater, ">"), false),
      (3, 2, (TokenType.Greater, ">"), true),
      (3, 5, (TokenType.GreaterEqual, ">="), false),
      (3, 3, (TokenType.GreaterEqual, ">="), true),
      (3, 2, (TokenType.GreaterEqual, ">="), true),
    )
    forAll(table): (left, right, operator, expected) =>
      testSimpleBinaryExpr(
        left = LoxDataType.Number(left),
        right = LoxDataType.Number(right),
        operator = operator,
        expected = LoxDataType.Bool(expected),
      )

  test("==, !="):
    val table = Table(
      ("left", "right", "operator", "expected"),
      (LoxDataType.Number(3), LoxDataType.Number(3), (TokenType.EqualEqual, "=="), true),
      (LoxDataType.Number(3), LoxDataType.Number(5), (TokenType.EqualEqual, "=="), false),
      (LoxDataType.Number(3), LoxDataType.Number(3), (TokenType.BangEqual, "!="), false),
      (LoxDataType.Number(3), LoxDataType.Number(5), (TokenType.BangEqual, "!="), true),
      (LoxDataType.String("foo"), LoxDataType.String("foo"), (TokenType.EqualEqual, "=="), true),
      (LoxDataType.String("foo"), LoxDataType.String("bar"), (TokenType.EqualEqual, "=="), false),
      (LoxDataType.String("foo"), LoxDataType.String("foo"), (TokenType.BangEqual, "!="), false),
      (LoxDataType.String("foo"), LoxDataType.String("bar"), (TokenType.BangEqual, "!="), true),
      (LoxDataType.Bool(true), LoxDataType.Bool(true), (TokenType.EqualEqual, "=="), true),
      (LoxDataType.Bool(true), LoxDataType.Bool(false), (TokenType.EqualEqual, "=="), false),
      (LoxDataType.Number(3), LoxDataType.String("3"), (TokenType.EqualEqual, "=="), false),
      (LoxDataType.String(""), LoxDataType.Nil, (TokenType.EqualEqual, "=="), false),
      (LoxDataType.Nil, LoxDataType.Number(0), (TokenType.EqualEqual, "=="), false),
      (LoxDataType.Nil, LoxDataType.Nil, (TokenType.EqualEqual, "=="), true),
    )
    forAll(table): (left, right, operator, expected) =>
      testSimpleBinaryExpr(
        left = left,
        right = right,
        operator = operator,
        expected = LoxDataType.Bool(expected),
      )

  test("!"):
    val table = Table(
      ("right", "operator", "expected"),
      (LoxDataType.Bool(true), (TokenType.Bang, "!"), false),
      (LoxDataType.Bool(false), (TokenType.Bang, "!"), true),
      (LoxDataType.String(""), (TokenType.Bang, "!"), false),
      (LoxDataType.String("foo"), (TokenType.Bang, "!"), false),
      (LoxDataType.Number(0), (TokenType.Bang, "!"), false),
      (LoxDataType.Nil, (TokenType.Bang, "!"), true),
    )
    forAll(table): (right, operator, expected) =>
      testSimpleUnaryExpr(
        right = right,
        operator = operator,
        expected = LoxDataType.Bool(expected),
      )

  test("Number unary -"):
    val table = Table(
      ("right", "operator", "expected"),
      (1.23, (TokenType.Minus, "-"), -1.23),
      (-10.0, (TokenType.Minus, "-"), 10.0),
      (0.0, (TokenType.Minus, "-"), -0.0),
    )
    forAll(table): (right, operator, expected) =>
      testSimpleUnaryExpr(
        right = LoxDataType.Number(right),
        operator = operator,
        expected = LoxDataType.Number(expected),
      )

  test("複数の文"):
    val sut = Interpreter()
    val stmts = Seq(
      Stmt.Print(
        expression = Expr.Literal(LoxDataType.Bool(true)),
      ),
      Stmt.Print(
        expression = Expr.Literal(LoxDataType.Nil),
      ),
      Stmt.Print(
        expression = Expr.Literal(LoxDataType.String("foo")),
      ),
    )
    Resolver(sut).resolve(stmts)
    val expected = "true\nnil\nfoo\n"
    assertOutput(sut.interpret(stmts))(expected)

  test("var and assign"):
    val sut = Interpreter()
    val stmts = Seq(
      Stmt.Var(
        name = token(TokenType.Identifier, "a"),
        initializer = Some(Expr.Literal(LoxDataType.Number(3))),
      ),
      Stmt.Var(
        name = token(TokenType.Identifier, "b"),
        initializer = None,
      ),
      Stmt.Print(
        expression = Expr.Variable(token(TokenType.Identifier, "a")),
      ),
      Stmt.Print(
        expression = Expr.Variable(token(TokenType.Identifier, "b")),
      ),
      Stmt.Expression(
        expression = Expr.Assign(
          name = token(TokenType.Identifier, "a"),
          value = Expr.Literal(LoxDataType.Number(5)),
        ),
      ),
      Stmt.Print(
        expression = Expr.Variable(token(TokenType.Identifier, "a")),
      ),
      Stmt.Expression(
        expression = Expr.Assign(
          name = token(TokenType.Identifier, "a"),
          value = Expr.Assign(
            name = token(TokenType.Identifier, "b"),
            value = Expr.Literal(LoxDataType.String("foo")),
          ),
        ),
      ),
      Stmt.Print(
        expression = Expr.Binary(
          left = Expr.Variable(token(TokenType.Identifier, "a")),
          operator = token(TokenType.Plus, "+"),
          right = Expr.Variable(token(TokenType.Identifier, "b")),
        ),
      ),
    )
    Resolver(sut).resolve(stmts)
    val expected = "3\nnil\n5\nfoofoo\n"
    assertOutput(sut.interpret(stmts))(expected)

  test("scope"):
    val sut = Interpreter()
    val stmts = Seq(
      varStmt("a", "global a"),
      varStmt("b", "global b"),
      varStmt("c", "global c"),
      blockStmt(
        varStmt("a", "outer a"),
        varStmt("b", "outer b"),
        blockStmt(
          varStmt("a", "inner a"),
          printVarStmt("a"),
          printVarStmt("b"),
          printVarStmt("c"),
        ),
        printVarStmt("a"),
        printVarStmt("b"),
        printVarStmt("c"),
      ),
      printVarStmt("a"),
      printVarStmt("b"),
      printVarStmt("c"),
    )
    Resolver(sut).resolve(stmts)
    val expected =
      """inner a
        |outer b
        |global c
        |outer a
        |outer b
        |global c
        |global a
        |global b
        |global c
        |""".stripMargin
    assertOutput(sut.interpret(stmts))(expected)

  test("if"):
    val sut = Interpreter()
    val stmts = Seq(
      Stmt.If(
        condition = Expr.Literal(LoxDataType.Bool(true)),
        thenBranch = printLiteralStmt(1),
        elseBranch = None,
      ),
      Stmt.If(
        condition = Expr.Literal(LoxDataType.Bool(false)),
        thenBranch = printLiteralStmt(2),
        elseBranch = None,
      ),
      Stmt.If(
        condition = Expr.Literal(LoxDataType.Number(0)),
        thenBranch = printLiteralStmt(3),
        elseBranch = Some(printLiteralStmt(4)),
      ),
      Stmt.If(
        condition = Expr.Literal(LoxDataType.Nil),
        thenBranch = printLiteralStmt(5),
        elseBranch = Some(printLiteralStmt(6)),
      ),
    )
    Resolver(sut).resolve(stmts)
    val expected =
      """1
        |3
        |6
        |""".stripMargin
    assertOutput(sut.interpret(stmts))(expected)

  test("and, or"):
    val sut = Interpreter()
    val stmts = Seq(
      Stmt.Print(
        expression = Expr.Logical(
          left = literal(true),
          operator = token(TokenType.And, "and"),
          right = literal(1),
        ),
      ),
      Stmt.Print(
        expression = Expr.Logical(
          left = literal(false),
          operator = token(TokenType.And, "and"),
          right = literal(2),
        ),
      ),
      Stmt.Print(
        expression = Expr.Logical(
          left = literal("foo"),
          operator = token(TokenType.Or, "or"),
          right = literal("bar"),
        ),
      ),
      Stmt.Print(
        expression = Expr.Logical(
          left = literal("nil"),
          operator = token(TokenType.Or, "or"),
          right = literal(3),
        ),
      ),
    )
    Resolver(sut).resolve(stmts)
    val expected =
      """1
        |false
        |foo
        |3
        |""".stripMargin
    assertOutput(sut.interpret(stmts))(expected)

  test("while"):
    val sut = Interpreter()
    val stmts = Seq(
      varStmt("i", 1),
      Stmt.While(
        condition = Expr.Binary(
          left = Expr.Variable(token(TokenType.Identifier, "i")),
          operator = token(TokenType.Less, "<"),
          right = literal(5),
        ),
        body = blockStmt(
          printVarStmt("i"),
          Stmt.Expression(
            expression = Expr.Assign(
              name = token(TokenType.Identifier, "i"),
              value = Expr.Binary(
                left = Expr.Variable(token(TokenType.Identifier, "i")),
                operator = token(TokenType.Plus, "+"),
                right = literal(1),
              ),
            ),
          ),
        ),
      ),
    )
    Resolver(sut).resolve(stmts)
    val expected =
      """1
        |2
        |3
        |4
        |""".stripMargin
    assertOutput(sut.interpret(stmts))(expected)

  private def testSimpleBinaryExpr(
      left: LoxDataType,
      right: LoxDataType,
      operator: (TokenType, String),
      expected: LoxDataType | Unit,
  ): Unit =
    val sut = Interpreter()
    val expr = Expr.Binary(
      left = Expr.Literal(left),
      operator = token(operator._1, operator._2),
      right = Expr.Literal(right),
    )
    val stmts = Seq(Stmt.Print(expr))
    Resolver(sut).resolve(stmts)
    assertOutput(sut.interpret(stmts))(expected.toString() + "\n")

  private def testSimpleUnaryExpr(
      right: LoxDataType,
      operator: (TokenType, String),
      expected: LoxDataType | Unit,
  ): Unit =
    val sut = Interpreter()
    val expr = Expr.Unary(
      operator = token(operator._1, operator._2),
      right = Expr.Literal(right),
    )
    val stmts = Seq(Stmt.Print(expr))
    Resolver(sut).resolve(stmts)
    assertOutput(sut.interpret(stmts))(expected.toString() + "\n")

  private def varStmt(
      name: String,
      value: "nil" | Double | String | Boolean,
  ): Stmt.Var =
    Stmt.Var(
      name = token(TokenType.Identifier, name),
      initializer = Some(literal(value)),
    )

  private def printVarStmt(
      name: String,
  ): Stmt.Print =
    Stmt.Print(
      expression = Expr.Variable(token(TokenType.Identifier, name)),
    )

  private def blockStmt(
      statements: Stmt*,
  ): Stmt.Block =
    Stmt.Block(statements)

  private def printLiteralStmt(
      value: "nil" | Double | String | Boolean,
  ): Stmt.Print =
    Stmt.Print(
      expression = literal(value),
    )

  private def literal(
      value: "nil" | Double | String | Boolean,
  ): Expr.Literal =
    val literal = value match
      case v: "nil"   => LoxDataType.Nil
      case v: Double  => LoxDataType.Number(v)
      case v: String  => LoxDataType.String(v)
      case v: Boolean => LoxDataType.Bool(v)
    Expr.Literal(literal)
