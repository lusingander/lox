package jlox

class Interpreter extends Expr.Visitor[LoxDataType] with Stmt.Visitor[Unit]:

  private val environment = Environment()

  def interpret(statements: Seq[Stmt]): Unit =
    try statements.foreach(execute)
    catch case e: RuntimeError => Lox.runtimeError(e)

  private def evaluate(expr: Expr): LoxDataType =
    expr.accept(this)

  private def execute(stmt: Stmt): Unit =
    stmt.accept(this)

  override def visitExpressionStmt(stmt: Stmt.Expression): Unit =
    evaluate(stmt.expression)

  override def visitPrintStmt(stmt: Stmt.Print): Unit =
    val value = evaluate(stmt.expression)
    println(value)

  override def visitVarStmt(stmt: Stmt.Var): Unit =
    val value = stmt.initializer match
      case Some(initializer) => evaluate(initializer)
      case None              => LoxDataType.Nil
    environment.define(stmt.name.lexeme, value)

  override def visitBinaryExpr(expr: Expr.Binary): LoxDataType =
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

  override def visitGroupingExpr(expr: Expr.Grouping): LoxDataType =
    evaluate(expr.expression)

  override def visitLiteralExpr(expr: Expr.Literal): LoxDataType =
    expr.value

  override def visitUnaryExpr(expr: Expr.Unary): LoxDataType =
    val right = evaluate(expr.right)
    expr.operator.tp match
      case TokenType.Bang => LoxDataType.Bool(!isTruthy(right))
      case TokenType.Minus =>
        right match
          case LoxDataType.Number(v) => LoxDataType.Number(-v)
          case _ => throw RuntimeError(expr.operator, "Operand must be a number.")
      case _ => throw RuntimeError(expr.operator, s"Unexpected operator: ${expr.operator.tp}")

  override def visitVariableExpr(expr: Expr.Variable): LoxDataType =
    environment.get(expr.name)

  private def isTruthy(obj: LoxDataType): Boolean =
    obj match
      case LoxDataType.Number(_) => true
      case LoxDataType.String(_) => true
      case LoxDataType.Bool(v)   => v
      case LoxDataType.Nil       => false
