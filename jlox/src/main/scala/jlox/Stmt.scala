package jlox

sealed trait Stmt:
  def accept[R](visitor: Stmt.Visitor[R]): R

object Stmt:
  case class Block(statements: Seq[Stmt]) extends Stmt:
    override def accept[R](visitor: Visitor[R]): R =
      visitor.visitBlockStmt(this)

  case class Expression(expression: Expr) extends Stmt:
    override def accept[R](visitor: Visitor[R]): R =
      visitor.visitExpressionStmt(this)

  case class Print(expression: Expr) extends Stmt:
    override def accept[R](visitor: Visitor[R]): R =
      visitor.visitPrintStmt(this)

  case class Var(name: Token, initializer: Option[Expr]) extends Stmt:
    override def accept[R](visitor: Visitor[R]): R =
      visitor.visitVarStmt(this)

  trait Visitor[R]:
    def visitBlockStmt(stmt: Stmt.Block): R
    def visitExpressionStmt(stmt: Stmt.Expression): R
    def visitPrintStmt(stmt: Stmt.Print): R
    def visitVarStmt(stmt: Stmt.Var): R
