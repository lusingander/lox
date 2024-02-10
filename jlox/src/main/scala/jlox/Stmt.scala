package jlox

sealed trait Stmt:
  def accept[R](visitor: Stmt.Visitor[R]): Environment ?=> R

object Stmt:
  case class Block(statements: Seq[Stmt]) extends Stmt:
    override def accept[R](visitor: Visitor[R]): Environment ?=> R =
      visitor.visitBlockStmt(this)

  case class Class(name: Token, superclass: Option[Expr.Variable], methods: Seq[Stmt.Function]) extends Stmt:
    override def accept[R](visitor: Visitor[R]): Environment ?=> R =
      visitor.visitClassStmt(this)

  case class Expression(expression: Expr) extends Stmt:
    override def accept[R](visitor: Visitor[R]): Environment ?=> R =
      visitor.visitExpressionStmt(this)

  case class Function(name: Token, params: Seq[Token], body: Seq[Stmt]) extends Stmt:
    override def accept[R](visitor: Visitor[R]): Environment ?=> R =
      visitor.visitFunctionStmt(this)

  case class If(condition: Expr, thenBranch: Stmt, elseBranch: Option[Stmt]) extends Stmt:
    override def accept[R](visitor: Visitor[R]): Environment ?=> R =
      visitor.visitIfStmt(this)

  case class Print(expression: Expr) extends Stmt:
    override def accept[R](visitor: Visitor[R]): Environment ?=> R =
      visitor.visitPrintStmt(this)

  case class Return(keyword: Token, value: Option[Expr]) extends Stmt:
    override def accept[R](visitor: Visitor[R]): Environment ?=> R =
      visitor.visitReturnStmt(this)

  case class Var(name: Token, initializer: Option[Expr]) extends Stmt:
    override def accept[R](visitor: Visitor[R]): Environment ?=> R =
      visitor.visitVarStmt(this)

  case class While(condition: Expr, body: Stmt) extends Stmt:
    override def accept[R](visitor: Visitor[R]): Environment ?=> R =
      visitor.visitWhileStmt(this)

  trait Visitor[R]:
    def visitBlockStmt(stmt: Stmt.Block): Environment ?=> R
    def visitClassStmt(stmt: Stmt.Class): Environment ?=> R
    def visitExpressionStmt(stmt: Stmt.Expression): Environment ?=> R
    def visitFunctionStmt(stmt: Stmt.Function): Environment ?=> R
    def visitIfStmt(stmt: Stmt.If): Environment ?=> R
    def visitPrintStmt(stmt: Stmt.Print): Environment ?=> R
    def visitReturnStmt(stmt: Stmt.Return): Environment ?=> R
    def visitVarStmt(stmt: Stmt.Var): Environment ?=> R
    def visitWhileStmt(stmt: Stmt.While): Environment ?=> R
