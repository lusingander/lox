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

  case class Grouping(expression: Expr) extends Expr:
    override def accept[R](visitor: Visitor[R]): Environment ?=> R =
      visitor.visitGroupingExpr(this)

  case class Literal(value: LoxDataType) extends Expr:
    override def accept[R](visitor: Visitor[R]): Environment ?=> R =
      visitor.visitLiteralExpr(this)

  case class Unary(operator: Token, right: Expr) extends Expr:
    override def accept[R](visitor: Visitor[R]): Environment ?=> R =
      visitor.visitUnaryExpr(this)

  case class Variable(name: Token) extends Expr:
    override def accept[R](visitor: Visitor[R]): Environment ?=> R =
      visitor.visitVariableExpr(this)

  trait Visitor[R]:
    def visitAssignExpr(expr: Expr.Assign): Environment ?=> R
    def visitBinaryExpr(expr: Expr.Binary): Environment ?=> R
    def visitGroupingExpr(expr: Expr.Grouping): Environment ?=> R
    def visitLiteralExpr(expr: Expr.Literal): Environment ?=> R
    def visitUnaryExpr(expr: Expr.Unary): Environment ?=> R
    def visitVariableExpr(expr: Expr.Variable): Environment ?=> R
