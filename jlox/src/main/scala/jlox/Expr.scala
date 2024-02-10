package jlox

sealed trait Expr:
  def accept[R](visitor: Expr.Visitor[R]): Environment ?=> R

object Expr:
  case class Assign(name: Token, value: Expr) extends Expr:
    override def accept[R](visitor: Visitor[R]): Environment ?=> R =
      visitor.visitAssignExpr(this)

  case class Binary(left: Expr, operator: Token, right: Expr) extends Expr:
    override def accept[R](visitor: Visitor[R]): Environment ?=> R =
      visitor.visitBinaryExpr(this)

  case class Call(callee: Expr, paren: Token, arguments: Seq[Expr]) extends Expr:
    override def accept[R](visitor: Visitor[R]): Environment ?=> R =
      visitor.visitCallExpr(this)

  case class Get(obj: Expr, name: Token) extends Expr:
    override def accept[R](visitor: Visitor[R]): Environment ?=> R =
      visitor.visitGetExpr(this)

  case class Grouping(expression: Expr) extends Expr:
    override def accept[R](visitor: Visitor[R]): Environment ?=> R =
      visitor.visitGroupingExpr(this)

  case class Literal(value: LoxDataType) extends Expr:
    override def accept[R](visitor: Visitor[R]): Environment ?=> R =
      visitor.visitLiteralExpr(this)

  case class Logical(left: Expr, operator: Token, right: Expr) extends Expr:
    override def accept[R](visitor: Visitor[R]): Environment ?=> R =
      visitor.visitLogicalExpr(this)

  case class Set(obj: Expr, name: Token, value: Expr) extends Expr:
    override def accept[R](visitor: Visitor[R]): Environment ?=> R =
      visitor.visitSetExpr(this)

  case class Super(keyword: Token, method: Token) extends Expr:
    override def accept[R](visitor: Visitor[R]): Environment ?=> R =
      visitor.visitSuperExpr(this)

  case class This(keyword: Token) extends Expr:
    override def accept[R](visitor: Visitor[R]): Environment ?=> R =
      visitor.visitThisExpr(this)

  case class Unary(operator: Token, right: Expr) extends Expr:
    override def accept[R](visitor: Visitor[R]): Environment ?=> R =
      visitor.visitUnaryExpr(this)

  case class Variable(name: Token) extends Expr:
    override def accept[R](visitor: Visitor[R]): Environment ?=> R =
      visitor.visitVariableExpr(this)

  trait Visitor[R]:
    def visitAssignExpr(expr: Expr.Assign): Environment ?=> R
    def visitBinaryExpr(expr: Expr.Binary): Environment ?=> R
    def visitCallExpr(expr: Expr.Call): Environment ?=> R
    def visitGetExpr(expr: Expr.Get): Environment ?=> R
    def visitGroupingExpr(expr: Expr.Grouping): Environment ?=> R
    def visitLiteralExpr(expr: Expr.Literal): Environment ?=> R
    def visitLogicalExpr(expr: Expr.Logical): Environment ?=> R
    def visitSetExpr(expr: Expr.Set): Environment ?=> R
    def visitSuperExpr(expr: Expr.Super): Environment ?=> R
    def visitThisExpr(expr: Expr.This): Environment ?=> R
    def visitUnaryExpr(expr: Expr.Unary): Environment ?=> R
    def visitVariableExpr(expr: Expr.Variable): Environment ?=> R
