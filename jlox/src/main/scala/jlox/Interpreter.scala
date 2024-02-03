package jlox

class Interpreter extends Expr.Visitor[LoxDataType]:

  def interpret(expression: Expr): LoxDataType | Unit =
    try evaluate(expression)
    catch case e: RuntimeError => Lox.runtimeError(e)

  private def evaluate(expr: Expr): LoxDataType =
    expr.accept(this)

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

  private def isTruthy(obj: LoxDataType): Boolean =
    obj match
      case LoxDataType.Number(_) => true
      case LoxDataType.String(_) => true
      case LoxDataType.Bool(v)   => v
      case LoxDataType.Nil       => false
