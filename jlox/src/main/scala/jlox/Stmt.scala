package jlox

sealed trait Stmt:
  def accept[R](visitor: Stmt.Visitor[R]): Environment ?=> R

object Stmt:
  case class Block(statements: Seq[Stmt]) extends Stmt:
    override def accept[R](visitor: Visitor[R]): Environment ?=> R =
      visitor.visitBlockStmt(this)

  case class Expression(expression: Expr) extends Stmt:
    override def accept[R](visitor: Visitor[R]): Environment ?=> R =
      visitor.visitExpressionStmt(this)

  case class Print(expression: Expr) extends Stmt:
    override def accept[R](visitor: Visitor[R]): Environment ?=> R =
      visitor.visitPrintStmt(this)

  case class Var(name: Token, initializer: Option[Expr]) extends Stmt:
    override def accept[R](visitor: Visitor[R]): Environment ?=> R =
      visitor.visitVarStmt(this)

  trait Visitor[R]:
    def visitBlockStmt(stmt: Stmt.Block): Environment ?=> R
    def visitExpressionStmt(stmt: Stmt.Expression): Environment ?=> R
    def visitPrintStmt(stmt: Stmt.Print): Environment ?=> R
    def visitVarStmt(stmt: Stmt.Var): Environment ?=> R
