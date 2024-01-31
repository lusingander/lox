package jlox

sealed trait Expr:
  def accept[R](visitor: Expr.Visitor[R]): R

object Expr:
  case class Binary(left: Expr, operator: Token, right: Expr) extends Expr:
    override def accept[R](visitor: Visitor[R]): R =
      visitor.visitBinaryExpr(this)

  case class Grouping(expression: Expr) extends Expr:
    override def accept[R](visitor: Visitor[R]): R =
      visitor.visitGroupingExpr(this)

  case class Literal(value: Option[LiteralType]) extends Expr:
    override def accept[R](visitor: Visitor[R]): R =
      visitor.visitLiteralExpr(this)

  case class Unary(operator: Token, right: Expr) extends Expr:
    override def accept[R](visitor: Visitor[R]): R =
      visitor.visitUnaryExpr(this)

  trait Visitor[R]:
    def visitBinaryExpr(expr: Expr.Binary): R
    def visitGroupingExpr(expr: Expr.Grouping): R
    def visitLiteralExpr(expr: Expr.Literal): R
    def visitUnaryExpr(expr: Expr.Unary): R
