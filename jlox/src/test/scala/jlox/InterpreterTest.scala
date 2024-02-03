package jlox

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class InterpreterTest extends AnyFunSuite with TableDrivenPropertyChecks:

  test("Number grouping +, *"):
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
      (0.0, (TokenType.Minus, "-"), 0.0),
    )
    forAll(table): (right, operator, expected) =>
      testSimpleUnaryExpr(
        right = LoxDataType.Number(right),
        operator = operator,
        expected = LoxDataType.Number(expected),
      )

  private def testSimpleBinaryExpr(
      left: LoxDataType,
      right: LoxDataType,
      operator: (TokenType, String),
      expected: LoxDataType | Unit,
  ): Unit =
    val sut = Interpreter()
    val expr = Expr.Binary(
      left = Expr.Literal(left),
      operator = Token(operator._1, operator._2, LoxDataType.Nil, 1),
      right = Expr.Literal(right),
    )
    val actual = sut.interpret(expr)
    assert(actual == expected)

  private def testSimpleUnaryExpr(
      right: LoxDataType,
      operator: (TokenType, String),
      expected: LoxDataType | Unit,
  ): Unit =
    val sut = Interpreter()
    val expr = Expr.Unary(
      operator = Token(operator._1, operator._2, LoxDataType.Nil, 1),
      right = Expr.Literal(right),
    )
    val actual = sut.interpret(expr)
    assert(actual == expected)
