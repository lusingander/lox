package jlox

import scala.collection.mutable

class Resolver(
    private val interpreter: Interpreter,
) extends Expr.Visitor[Unit]
    with Stmt.Visitor[Unit]:

  import Resolver.*

  private given dummyEnvironment: Environment = Environment()

  private val scopes: mutable.Stack[mutable.Map[String, Boolean]] = mutable.Stack.empty
  private var currentFunction: FunctionType = FunctionType.None
  private var currentClass: ClassType = ClassType.None

  def resolve(statements: Seq[Stmt]): Unit =
    statements.foreach(resolve)

  private def resolve(stmt: Stmt): Unit =
    stmt.accept(this)

  private def resolve(expr: Expr): Unit =
    expr.accept(this)

  private def resolveFunction(function: Stmt.Function, ft: FunctionType): Unit =
    val enclosingFunction = currentFunction
    currentFunction = ft

    beginScope()
    function.params.foreach: param =>
      declare(param)
      define(param)
    resolve(function.body)
    endScope()

    currentFunction = enclosingFunction

  private def beginScope(): Unit =
    scopes.push(mutable.Map.empty)

  private def endScope(): Unit =
    scopes.pop()

  private def declare(name: Token): Unit =
    scopes.headOption.foreach: scope =>
      if scope.contains(name.lexeme) then
        Lox.error(name, "Already a variable with this namein this scope.")
      scope.put(name.lexeme, false)

  private def define(name: Token): Unit =
    scopes.headOption.foreach: scope =>
      scope.put(name.lexeme, true)

  private def resolveLocal(expr: Expr, name: Token): Unit =
    scopes.zipWithIndex
      .find: (scope, _) =>
        scope.contains(name.lexeme)
      .foreach: (_, i) =>
        interpreter.resolve(expr, i)

  override def visitBlockStmt(stmt: Stmt.Block): Environment ?=> Unit =
    beginScope()
    resolve(stmt.statements)
    endScope()

  override def visitClassStmt(stmt: Stmt.Class): Environment ?=> Unit =
    val enclosingClass = currentClass
    currentClass = ClassType.Class

    declare(stmt.name)
    define(stmt.name)

    stmt.superclass.foreach: sc =>
      if sc.name.lexeme == stmt.name.lexeme then
        Lox.error(sc.name, "A class can't inherit from itself.")
    stmt.superclass.foreach: sc =>
      currentClass = ClassType.SubClass
      resolve(sc)

    stmt.superclass.foreach: _ =>
      beginScope()
      scopes.headOption.foreach: scope =>
        scope.put("super", true)

    beginScope()
    scopes.headOption.foreach: scope =>
      scope.put("this", true)
    stmt.methods.foreach: method =>
      val declaration =
        if method.name.lexeme == "init" then FunctionType.Initializer
        else FunctionType.Method
      resolveFunction(method, declaration)
    endScope()

    stmt.superclass.foreach: _ =>
      endScope()

    currentClass = enclosingClass

  override def visitExpressionStmt(stmt: Stmt.Expression): Environment ?=> Unit =
    resolve(stmt.expression)

  override def visitFunctionStmt(stmt: Stmt.Function): Environment ?=> Unit =
    declare(stmt.name)
    define(stmt.name)
    resolveFunction(stmt, FunctionType.Function)

  override def visitIfStmt(stmt: Stmt.If): Environment ?=> Unit =
    resolve(stmt.condition)
    resolve(stmt.thenBranch)
    stmt.elseBranch.foreach(resolve)

  override def visitPrintStmt(stmt: Stmt.Print): Environment ?=> Unit =
    resolve(stmt.expression)

  override def visitReturnStmt(stmt: Stmt.Return): Environment ?=> Unit =
    if currentFunction == FunctionType.None then
      Lox.error(stmt.keyword, "Can't return from top-level code.")
    stmt.value.foreach: v =>
      if currentFunction == FunctionType.Initializer then
        Lox.error(stmt.keyword, "Can't return a value from an initializer.")
      resolve(v)

  override def visitVarStmt(stmt: Stmt.Var): Environment ?=> Unit =
    declare(stmt.name)
    stmt.initializer.foreach: init =>
      resolve(init)
    define(stmt.name)

  override def visitWhileStmt(stmt: Stmt.While): Environment ?=> Unit =
    resolve(stmt.condition)
    resolve(stmt.body)

  override def visitAssignExpr(expr: Expr.Assign): Environment ?=> Unit =
    resolve(expr.value)
    resolveLocal(expr, expr.name)

  override def visitBinaryExpr(expr: Expr.Binary): Environment ?=> Unit =
    resolve(expr.left)
    resolve(expr.right)

  override def visitCallExpr(expr: Expr.Call): Environment ?=> Unit =
    resolve(expr.callee)
    expr.arguments.foreach(resolve)

  override def visitGetExpr(expr: Expr.Get): Environment ?=> Unit =
    resolve(expr.obj)

  override def visitGroupingExpr(expr: Expr.Grouping): Environment ?=> Unit =
    resolve(expr.expression)

  override def visitLiteralExpr(expr: Expr.Literal): Environment ?=> Unit =
    ()

  override def visitLogicalExpr(expr: Expr.Logical): Environment ?=> Unit =
    resolve(expr.left)
    resolve(expr.right)

  override def visitSetExpr(expr: Expr.Set): Environment ?=> Unit =
    resolve(expr.value)
    resolve(expr.obj)

  override def visitSuperExpr(expr: Expr.Super): Environment ?=> Unit =
    if currentClass == ClassType.None then
      Lox.error(expr.keyword, "Can't use 'super' outside of a class.")
    else if currentClass != ClassType.SubClass then
      Lox.error(expr.keyword, "Can't use 'super' in a class with no superclass.")
    resolveLocal(expr, expr.keyword)

  override def visitThisExpr(expr: Expr.This): Environment ?=> Unit =
    if currentClass == ClassType.None then
      Lox.error(expr.keyword, "Can't use 'this' outside of a class.")
    else resolveLocal(expr, expr.keyword)

  override def visitUnaryExpr(expr: Expr.Unary): Environment ?=> Unit =
    resolve(expr.right)

  override def visitVariableExpr(expr: Expr.Variable): Environment ?=> Unit =
    if scopes.headOption.flatMap(_.get(expr.name.lexeme)).exists(_ == false) then
      Lox.error(expr.name, "Can't read local variable in its own initializer.")
    resolveLocal(expr, expr.name)

object Resolver:
  private enum FunctionType:
    case None, Function, Initializer, Method

  private enum ClassType:
    case None, Class, SubClass
