package jlox

class Parser(
    val tokens: Seq[Token],
    var current: Int = 0,
):
  import Parser.*

  def parse(): Expr =
    try expression()
    catch case e: ParseError => null

  private def expression(): Expr =
    equality()

  private def equality(): Expr =
    var expr = comparison()
    while `match`(TokenType.BangEqual, TokenType.EqualEqual) do
      val operator = previous()
      val right = comparison()
      expr = Expr.Binary(expr, operator, right)
    expr

  private def comparison(): Expr =
    var expr = term()
    while `match`(
        TokenType.Greater,
        TokenType.GreaterEqual,
        TokenType.LessEqual,
        TokenType.Less,
      )
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
    if `match`(TokenType.False) then Expr.Literal(Some(false))
    else if `match`(TokenType.True) then Expr.Literal(Some(true))
    else if `match`(TokenType.Nil) then Expr.Literal(None)
    else if `match`(TokenType.Number, TokenType.String) then
      Expr.Literal(previous().literal)
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
        case TokenType.Class | TokenType.For | TokenType.Fun | TokenType.If |
            TokenType.Print | TokenType.Return | TokenType.Var |
            TokenType.While =>
          return
        case _ => advance()

object Parser:
  class ParseError extends RuntimeException
