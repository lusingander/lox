package jlox

import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable

class ExprTest extends AnyFunSuite:

  test("AstPrinter"):
    val expr = Expr.Binary(
      left = Expr.Unary(
        operator = Token(TokenType.Minus, "-", None, 1),
        right = Expr.Literal(123),
      ),
      operator = Token(TokenType.Star, "*", None, 1),
      right = Expr.Grouping(
        expression = Expr.Literal(45.67),
      ),
    )
    val expected = "(* (- 123) (group 45.67))"
    val sut = AstPrinter()
    val actual = sut.print(expr)
    assert(actual == expected)

  test("RpnPrinter"):
    val expr = Expr.Binary(
      left = Expr.Binary(
        left = Expr.Literal(1),
        operator = Token(TokenType.Plus, "+", None, 1),
        right = Expr.Literal(2),
      ),
      operator = Token(TokenType.Star, "*", None, 1),
      right = Expr.Binary(
        left = Expr.Literal(4),
        operator = Token(TokenType.Minus, "-", None, 1),
        right = Expr.Literal(3),
      ),
    )
    val expected = "1 2 + 4 3 - *"
    val sut = RpnPrinter()
    val actual = sut.print(expr)
    assert(actual == expected)

class AstPrinter extends Expr.Visitor[String]:

  def print(expr: Expr): String = expr.accept(this)

  override def visitBinaryExpr(expr: Expr.Binary): String =
    parenthesize(expr.operator.lexeme, expr.left, expr.right)

  override def visitGroupingExpr(expr: Expr.Grouping): String =
    parenthesize("group", expr.expression)

  override def visitLiteralExpr(expr: Expr.Literal): String =
    if expr.value == null then "nil" else expr.value.toString()

  override def visitUnaryExpr(expr: Expr.Unary): String =
    parenthesize(expr.operator.lexeme, expr.right)

  private def parenthesize(name: String, exprs: Expr*): String =
    val b = mutable.StringBuilder()
    b.append("(")
    b.append(name)
    exprs.foreach: e =>
      b.append(" ")
      b.append(e.accept(this))
    b.append(")")
    b.toString

class RpnPrinter extends Expr.Visitor[String]:

  def print(expr: Expr): String = expr.accept(this)

  override def visitBinaryExpr(expr: Expr.Binary): String =
    s"${expr.left.accept(this)} ${expr.right.accept(this)} ${expr.operator.lexeme}"

  override def visitGroupingExpr(expr: Expr.Grouping): String = ???

  override def visitLiteralExpr(expr: Expr.Literal): String =
    expr.value.toString()

  override def visitUnaryExpr(expr: Expr.Unary): String = ???
