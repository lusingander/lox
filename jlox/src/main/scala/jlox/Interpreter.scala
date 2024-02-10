package jlox

import scala.collection.mutable

class Interpreter extends Expr.Visitor[LoxDataType] with Stmt.Visitor[Unit]:

  import Interpreter.*

  private val global: Environment = Environment()
  global.define("clock", Global.clock)

  private given environment: Environment = global
  private val locals: mutable.Map[String, Int] = mutable.Map.empty

  def interpret(statements: Seq[Stmt]): Unit =
    try statements.foreach(execute)
    catch case e: RuntimeError => Lox.runtimeError(e)

  private def evaluate(expr: Expr): Environment ?=> LoxDataType =
    expr.accept(this)

  private def execute(stmt: Stmt): Environment ?=> Unit =
    stmt.accept(this)

  def resolve(expr: Expr, depth: Int): Unit =
    locals.put(genLocalsKey(expr), depth)

  def executeBlock(statements: Seq[Stmt]): Environment ?=> Unit =
    statements.foreach: statement =>
      execute(statement)

  override def visitBlockStmt(stmt: Stmt.Block): Environment ?=> Unit =
    executeBlock(stmt.statements)(using Environment(Some(summon[Environment])))

  override def visitClassStmt(stmt: Stmt.Class): Environment ?=> Unit =
    summon[Environment].define(stmt.name.lexeme, LoxDataType.Nil)
    val cls = LoxClass(stmt.name.lexeme)
    summon[Environment].assign(stmt.name, LoxDataType.Class(cls))

  override def visitExpressionStmt(stmt: Stmt.Expression): Environment ?=> Unit =
    evaluate(stmt.expression)

  override def visitFunctionStmt(stmt: Stmt.Function): Environment ?=> Unit =
    val function = LoxFunction(stmt, summon[Environment])
    summon[Environment].define(stmt.name.lexeme, LoxDataType.Function(function))

  override def visitIfStmt(stmt: Stmt.If): Environment ?=> Unit =
    if isTruthy(evaluate(stmt.condition)) then execute(stmt.thenBranch)
    else stmt.elseBranch.foreach(execute)

  override def visitPrintStmt(stmt: Stmt.Print): Environment ?=> Unit =
    val value = evaluate(stmt.expression)
    println(value)

  override def visitReturnStmt(stmt: Stmt.Return): Environment ?=> Unit =
    val value = stmt.value match
      case Some(v) => evaluate(v)
      case None    => LoxDataType.Nil
    throw Return(value)

  override def visitVarStmt(stmt: Stmt.Var): Environment ?=> Unit =
    val value = stmt.initializer match
      case Some(initializer) => evaluate(initializer)
      case None              => LoxDataType.Nil
    summon[Environment].define(stmt.name.lexeme, value)

  override def visitWhileStmt(stmt: Stmt.While): Environment ?=> Unit =
    while isTruthy(evaluate(stmt.condition)) do execute(stmt.body)

  override def visitAssignExpr(expr: Expr.Assign): Environment ?=> LoxDataType =
    val value = evaluate(expr.value)
    val distance = locals.get(genLocalsKey(expr))
    distance match
      case Some(dist) => summon[Environment].assignAt(dist, expr.name, value)
      case None       => global.assign(expr.name, value)
    value

  override def visitBinaryExpr(expr: Expr.Binary): Environment ?=> LoxDataType =
    val left = evaluate(expr.left)
    val right = evaluate(expr.right)
    expr.operator.tp match
      case TokenType.EqualEqual =>
        LoxDataType.Bool(left == right)
      case TokenType.BangEqual =>
        LoxDataType.Bool(left != right)
      case TokenType.Greater =>
        (left, right) match
          case (LoxDataType.Number(l), LoxDataType.Number(r)) => LoxDataType.Bool(l > r)
          case (_, _) => throw RuntimeError(expr.operator, "Operands must be numbers.")
      case TokenType.GreaterEqual =>
        (left, right) match
          case (LoxDataType.Number(l), LoxDataType.Number(r)) => LoxDataType.Bool(l >= r)
          case (_, _) => throw RuntimeError(expr.operator, "Operands must be numbers.")
      case TokenType.Less =>
        (left, right) match
          case (LoxDataType.Number(l), LoxDataType.Number(r)) => LoxDataType.Bool(l < r)
          case (_, _) => throw RuntimeError(expr.operator, "Operands must be numbers.")
      case TokenType.LessEqual =>
        (left, right) match
          case (LoxDataType.Number(l), LoxDataType.Number(r)) => LoxDataType.Bool(l <= r)
          case (_, _) => throw RuntimeError(expr.operator, "Operands must be numbers.")
      case TokenType.Minus =>
        (left, right) match
          case (LoxDataType.Number(l), LoxDataType.Number(r)) => LoxDataType.Number(l - r)
          case (_, _) => throw RuntimeError(expr.operator, "Operands must be numbers.")
      case TokenType.Plus =>
        (left, right) match
          case (LoxDataType.Number(l), LoxDataType.Number(r)) => LoxDataType.Number(l + r)
          case (LoxDataType.String(l), LoxDataType.String(r)) => LoxDataType.String(l + r)
          case (_, _) =>
            throw RuntimeError(expr.operator, "Operands must be two numbers or two strings.")
      case TokenType.Slash =>
        (left, right) match
          case (LoxDataType.Number(l), LoxDataType.Number(r)) => LoxDataType.Number(l / r)
          case (_, _) => throw RuntimeError(expr.operator, "Operands must be numbers.")
      case TokenType.Star =>
        (left, right) match
          case (LoxDataType.Number(l), LoxDataType.Number(r)) => LoxDataType.Number(l * r)
          case (_, _) => throw RuntimeError(expr.operator, "Operands must be numbers.")
      case _ => throw RuntimeError(expr.operator, s"Unexpected operator: ${expr.operator.tp}")

  override def visitCallExpr(expr: Expr.Call): Environment ?=> LoxDataType =
    val callee = evaluate(expr.callee)
    val arguments = expr.arguments.map(evaluate)

    callee match
      case LoxDataType.Function(function) =>
        if arguments.size != function.arity() then
          throw RuntimeError(
            expr.paren,
            s"Expected ${function.arity()} arguments but got ${arguments.size}.",
          )
        function.call(this, arguments)
      case LoxDataType.Class(cls) =>
        if arguments.size != cls.arity() then
          throw RuntimeError(
            expr.paren,
            s"Expected ${cls.arity()} arguments but got ${arguments.size}.",
          )
        cls.call(this, arguments)
      case _ => throw RuntimeError(expr.paren, "Can only call functions and classes.")

  override def visitGetExpr(expr: Expr.Get): Environment ?=> LoxDataType =
    val obj = evaluate(expr.obj)
    obj match
      case LoxDataType.Instance(instance) => instance.get(expr.name)
      case _ => throw RuntimeError(expr.name, "Only instances have properties.")

  override def visitGroupingExpr(expr: Expr.Grouping): Environment ?=> LoxDataType =
    evaluate(expr.expression)

  override def visitLiteralExpr(expr: Expr.Literal): Environment ?=> LoxDataType =
    expr.value

  override def visitLogicalExpr(expr: Expr.Logical): Environment ?=> LoxDataType =
    val left = evaluate(expr.left)
    expr.operator.tp match
      case TokenType.Or =>
        if isTruthy(left) then left else evaluate(expr.right)
      case TokenType.And =>
        if isTruthy(left) then evaluate(expr.right) else left
      case _ => throw RuntimeError(expr.operator, s"Unexpected operator: ${expr.operator.tp}")

  override def visitSetExpr(expr: Expr.Set): Environment ?=> LoxDataType =
    val obj = evaluate(expr.obj)
    obj match
      case LoxDataType.Instance(instance) =>
        val value = evaluate(expr.value)
        instance.set(expr.name, value)
        value
      case _ => throw RuntimeError(expr.name, "Only instances have fields.")

  override def visitUnaryExpr(expr: Expr.Unary): Environment ?=> LoxDataType =
    val right = evaluate(expr.right)
    expr.operator.tp match
      case TokenType.Bang => LoxDataType.Bool(!isTruthy(right))
      case TokenType.Minus =>
        right match
          case LoxDataType.Number(v) => LoxDataType.Number(-v)
          case _ => throw RuntimeError(expr.operator, "Operand must be a number.")
      case _ => throw RuntimeError(expr.operator, s"Unexpected operator: ${expr.operator.tp}")

  override def visitVariableExpr(expr: Expr.Variable): Environment ?=> LoxDataType =
    lookUpVariable(expr.name, expr)

  private def lookUpVariable(name: Token, expr: Expr): Environment ?=> LoxDataType =
    val distance = locals.get(genLocalsKey(expr))
    distance match
      case Some(dist) => summon[Environment].getAt(dist, name)
      case None       => global.get(name)

  private def isTruthy(obj: LoxDataType): Boolean =
    obj match
      case LoxDataType.Number(_)   => true
      case LoxDataType.String(_)   => true
      case LoxDataType.Bool(v)     => v
      case LoxDataType.Nil         => false
      case LoxDataType.Function(_) => true
      case LoxDataType.Class(_)    => true
      case LoxDataType.Instance(_) => true

  private def genLocalsKey(expr: Expr): String =
    // Get the hashCode for each instance according to the book... :(
    val h = System.identityHashCode(expr)
    h.toString() + expr.hashCode().toString()

object Interpreter:
  class Return(val value: LoxDataType) extends RuntimeException(null, null, false, false)
