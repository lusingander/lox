package jlox

import scala.collection.mutable
import jlox.Expr.Assign
import jlox.Expr.Binary
import jlox.Expr.Grouping
import jlox.Expr.Literal
import jlox.Expr.Unary
import jlox.Expr.Variable

class Parser(
    val tokens: Seq[Token],
):
  var current: Int = 0

  import Parser.*

  def parse(): Seq[Stmt] =
    val statements = mutable.ListBuffer.empty[Stmt]
    while !isAtEnd() do statements.addOne(declaration())
    statements.toSeq

  private def expression(): Expr =
    assignment()

  private def assignment(): Expr =
    val expr = or()
    if `match`(TokenType.Equal) then
      val equals = previous()
      val value = assignment()
      expr match
        case Variable(name) => Expr.Assign(name, value)
        case _ =>
          error(equals, "Invalid assignment target.")
          expr
    else expr

  private def or(): Expr =
    var expr = and()
    while `match`(TokenType.Or) do
      val operator = previous()
      val right = and()
      expr = Expr.Logical(expr, operator, right)
    expr

  private def and(): Expr =
    var expr = equality()
    while `match`(TokenType.And) do
      val operator = previous()
      val right = equality()
      expr = Expr.Logical(expr, operator, right)
    expr

  private def declaration(): Stmt =
    try
      if `match`(TokenType.Var) then varDeclaration()
      else statement()
    catch
      case e: ParseError =>
        synchronize()
        null

  private def statement(): Stmt =
    if `match`(TokenType.For) then forStatement()
    else if `match`(TokenType.If) then ifStatement()
    else if `match`(TokenType.Print) then printStatement()
    else if `match`(TokenType.While) then whileStatement()
    else if `match`(TokenType.LeftBrace) then Stmt.Block(block())
    else expressionStatement()

  private def forStatement(): Stmt =
    consume(TokenType.LeftParen, "Expect '(' after 'for'.")
    val initializer =
      if `match`(TokenType.Semicolon) then None
      else if `match`(TokenType.Var) then Some(varDeclaration())
      else Some(expressionStatement())
    val condition =
      if check(TokenType.Semicolon) then None
      else Some(expression())
    consume(TokenType.Semicolon, "Expect ';' after loop condition.")
    val increment =
      if check(TokenType.RightParen) then None
      else Some(expression())
    consume(TokenType.RightParen, "Expect ')' after for clauses.")

    val body = statement()

    val bodyPlusIncrement = increment match
      case Some(inc) => Stmt.Block(Seq(body, Stmt.Expression(inc)))
      case None      => body
    val conditionReplaced = condition match
      case Some(cond) => cond
      case None       => Expr.Literal(LoxDataType.Bool(true))

    val whileBody = Stmt.While(conditionReplaced, bodyPlusIncrement)

    initializer match
      case Some(init) => Stmt.Block(Seq(init, whileBody))
      case None       => whileBody

  private def ifStatement(): Stmt =
    consume(TokenType.LeftParen, "Expect '(' after 'if'.")
    val condition = expression()
    consume(TokenType.RightParen, "Expect ')' after if condition.")
    val thenBranch = statement()
    val elseBranch = if `match`(TokenType.Else) then Some(statement()) else None
    Stmt.If(condition, thenBranch, elseBranch)

  private def printStatement(): Stmt =
    val value = expression()
    consume(TokenType.Semicolon, "Expect ';' after value.")
    Stmt.Print(value)

  private def whileStatement(): Stmt =
    consume(TokenType.LeftParen, "Expect '(' after 'while'.")
    val condition = expression()
    consume(TokenType.RightParen, "Expect ')' after condition.")
    val body = statement()
    Stmt.While(condition, body)

  private def varDeclaration(): Stmt =
    val name = consume(TokenType.Identifier, "Expect variable name.")
    val initializer = if `match`(TokenType.Equal) then Some(expression()) else None
    consume(TokenType.Semicolon, "Expect ';' after variable declaration.")
    Stmt.Var(name, initializer)

  private def expressionStatement(): Stmt =
    val expr = expression()
    consume(TokenType.Semicolon, "Expect ';' after expression.")
    Stmt.Expression(expr)

  private def block(): Seq[Stmt] =
    val statements = mutable.ListBuffer.empty[Stmt]
    while !check(TokenType.RightBrace) && !isAtEnd() do statements.addOne(declaration())
    consume(TokenType.RightBrace, "Expect '}' after block.")
    statements.toSeq

  private def equality(): Expr =
    var expr = comparison()
    while `match`(TokenType.BangEqual, TokenType.EqualEqual) do
      val operator = previous()
      val right = comparison()
      expr = Expr.Binary(expr, operator, right)
    expr

  private def comparison(): Expr =
    var expr = term()
    while `match`(TokenType.Greater, TokenType.GreaterEqual, TokenType.LessEqual, TokenType.Less)
    do
      val operator = previous()
      val right = term()
      expr = Expr.Binary(expr, operator, right)
    expr

  private def term(): Expr =
    var expr = factor()
    while `match`(TokenType.Minus, TokenType.Plus) do
      val operator = previous()
      val right = factor()
      expr = Expr.Binary(expr, operator, right)
    expr

  private def factor(): Expr =
    var expr = unary()
    while `match`(TokenType.Slash, TokenType.Star) do
      val operator = previous()
      val right = unary()
      expr = Expr.Binary(expr, operator, right)
    expr

  private def unary(): Expr =
    if `match`(TokenType.Bang, TokenType.Minus) then
      val operator = previous()
      val right = unary()
      Expr.Unary(operator, right)
    else primary()

  private def primary(): Expr =
    if `match`(TokenType.False) then Expr.Literal(LoxDataType.Bool(false))
    else if `match`(TokenType.True) then Expr.Literal(LoxDataType.Bool(true))
    else if `match`(TokenType.Nil) then Expr.Literal(LoxDataType.Nil)
    else if `match`(TokenType.Number, TokenType.String) then Expr.Literal(previous().literal)
    else if `match`(TokenType.Identifier) then Expr.Variable(previous())
    else if `match`(TokenType.LeftParen) then
      val expr = expression()
      consume(TokenType.RightParen, "Expect ')' after expression.")
      Expr.Grouping(expr)
    else throw error(peek(), "Expect expression.")

  private def `match`(types: TokenType*): Boolean =
    types.find(check) match
      case Some(_) =>
        advance()
        true
      case None =>
        false

  private def consume(tp: TokenType, message: String): Token =
    if check(tp) then advance()
    else throw error(peek(), message)

  private def check(tp: TokenType): Boolean =
    !isAtEnd() && peek().tp == tp

  private def advance(): Token =
    if !isAtEnd() then current += 1
    previous()

  private def isAtEnd(): Boolean =
    peek().tp == TokenType.Eof

  private def peek(): Token =
    tokens(current)

  private def previous(): Token =
    tokens(current - 1)

  private def error(token: Token, message: String) =
    Lox.error(token, message)
    ParseError()

  private def synchronize(): Unit =
    advance()
    while !isAtEnd() do
      if previous().tp == TokenType.Semicolon then return
      peek().tp match
        case TokenType.Class | TokenType.For | TokenType.Fun | TokenType.If | TokenType.Print |
            TokenType.Return | TokenType.Var | TokenType.While =>
          return
        case _ => advance()

object Parser:
  class ParseError extends RuntimeException
