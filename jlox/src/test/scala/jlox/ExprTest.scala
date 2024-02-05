package jlox

import scala.collection.mutable

class ExprTest extends LoxTestBase:

  test("AstPrinter"):
    val expr = Expr.Binary(
      left = Expr.Unary(
        operator = Token(TokenType.Minus, "-", LoxDataType.Nil, 1),
        right = Expr.Literal(LoxDataType.Number(123)),
      ),
      operator = Token(TokenType.Star, "*", LoxDataType.Nil, 1),
      right = Expr.Grouping(
        expression = Expr.Literal(LoxDataType.Number(45.67)),
      ),
    )
    val expected = "(* (- 123) (group 45.67))"
    val sut = AstPrinter()
    val actual = sut.print(expr)
    assert(actual == expected)

  test("RpnPrinter"):
    val expr = Expr.Binary(
      left = Expr.Binary(
        left = Expr.Literal(LoxDataType.Number(1)),
        operator = Token(TokenType.Plus, "+", LoxDataType.Nil, 1),
        right = Expr.Literal(LoxDataType.Number(2)),
      ),
      operator = Token(TokenType.Star, "*", LoxDataType.Nil, 1),
      right = Expr.Binary(
        left = Expr.Literal(LoxDataType.Number(4)),
        operator = Token(TokenType.Minus, "-", LoxDataType.Nil, 1),
        right = Expr.Literal(LoxDataType.Number(3)),
      ),
    )
    val expected = "1 2 + 4 3 - *"
    val sut = RpnPrinter()
    val actual = sut.print(expr)
    assert(actual == expected)

class AstPrinter extends Expr.Visitor[String]:

  def print(expr: Expr): String = expr.accept(this)(using Environment())

  override def visitBinaryExpr(expr: Expr.Binary): Environment ?=> String =
    parenthesize(expr.operator.lexeme, expr.left, expr.right)

  override def visitGroupingExpr(expr: Expr.Grouping): Environment ?=> String =
    parenthesize("group", expr.expression)

  override def visitLiteralExpr(expr: Expr.Literal): Environment ?=> String =
    expr.value.toString()

  override def visitLogicalExpr(expr: Expr.Logical): Environment ?=> String = ???

  override def visitUnaryExpr(expr: Expr.Unary): Environment ?=> String =
    parenthesize(expr.operator.lexeme, expr.right)

  override def visitVariableExpr(expr: Expr.Variable): Environment ?=> String = ???

  override def visitAssignExpr(expr: Expr.Assign): Environment ?=> String = ???

  private def parenthesize(name: String, exprs: Expr*): Environment ?=> String =
    val b = mutable.StringBuilder()
    b.append("(")
    b.append(name)
    exprs.foreach: e =>
      b.append(" ")
      b.append(e.accept(this))
    b.append(")")
    b.toString

class RpnPrinter extends Expr.Visitor[String]:

  def print(expr: Expr): String = expr.accept(this)(using Environment())

  override def visitBinaryExpr(expr: Expr.Binary): Environment ?=> String =
    s"${expr.left.accept(this)} ${expr.right.accept(this)} ${expr.operator.lexeme}"

  override def visitGroupingExpr(expr: Expr.Grouping): Environment ?=> String = ???

  override def visitLiteralExpr(expr: Expr.Literal): Environment ?=> String =
    expr.value.toString()

  override def visitLogicalExpr(expr: Expr.Logical): Environment ?=> String = ???

  override def visitUnaryExpr(expr: Expr.Unary): Environment ?=> String = ???

  override def visitVariableExpr(expr: Expr.Variable): Environment ?=> String = ???

  override def visitAssignExpr(expr: Expr.Assign): Environment ?=> String = ???
