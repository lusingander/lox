package jlox

sealed trait Stmt:
  def accept[R](visitor: Stmt.Visitor[R]): R

object Stmt:
  case class Expression(expression: Expr) extends Stmt:
    override def accept[R](visitor: Visitor[R]): R =
      visitor.visitExpressionStmt(this)

  case class Print(expression: Expr) extends Stmt:
    override def accept[R](visitor: Visitor[R]): R =
      visitor.visitPrintStmt(this)

  trait Visitor[R]:
    def visitExpressionStmt(stmt: Stmt.Expression): R
    def visitPrintStmt(stmt: Stmt.Print): R
