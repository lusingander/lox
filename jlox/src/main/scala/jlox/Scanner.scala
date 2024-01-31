package jlox

import scala.collection.mutable

class Scanner(
    val source: String,
    val tokens: mutable.ListBuffer[Token] = mutable.ListBuffer.empty,
    var start: Int = 0,
    var current: Int = 0,
    var line: Int = 1,
):
  import Scanner.*

  def scanTokens(): Seq[Token] =
    while !isAtEnd() do
      start = current
      scanToken()
    tokens.addOne(Token(TokenType.Eof, "", None, line))
    tokens.toSeq

  private def scanToken(): Unit =
    val c = advance()
    c match
      case '(' => addToken(TokenType.LeftParen)
      case ')' => addToken(TokenType.RightParen)
      case '{' => addToken(TokenType.LeftBrace)
      case '}' => addToken(TokenType.RightBrace)
      case ',' => addToken(TokenType.Comma)
      case '.' => addToken(TokenType.Dot)
      case '-' => addToken(TokenType.Minus)
      case '+' => addToken(TokenType.Plus)
      case ';' => addToken(TokenType.Semicolon)
      case '*' => addToken(TokenType.Star)
      case '!' =>
        val tp = if `match`('=') then TokenType.BangEqual else TokenType.Bang
        addToken(tp)
      case '=' =>
        val tp = if `match`('=') then TokenType.EqualEqual else TokenType.Equal
        addToken(tp)
      case '<' =>
        val tp = if `match`('=') then TokenType.LessEqual else TokenType.Less
        addToken(tp)
      case '>' =>
        val tp =
          if `match`('=') then TokenType.GreaterEqual else TokenType.Greater
        addToken(tp)
      case '/' =>
        if `match`('/') then
          // comment
          while peek() != '\n' && !isAtEnd() do advance()
        else addToken(TokenType.Slash)
      case ' ' | '\r' | '\t' => // do nothing
      case '\n'              => line += 1
      case '"'               => string()
      case _ =>
        if isDigit(c) then number()
        else if isAlpha(c) then identifier()
        else Lox.error(line, s"Unexpected character `$c`")

  private def addToken(
      tp: TokenType,
      literal: Option[LiteralType] = None,
  ): Unit =
    val text = source.substring(start, current)
    tokens.addOne(Token(tp, text, literal, line))

  private def isAtEnd(): Boolean =
    source.size <= current

  private def advance(): Char =
    val before = current
    current += 1
    source.charAt(before)

  private def peek(): Char =
    if isAtEnd() then '\u0000'
    else source.charAt(current)

  private def peekNext(): Char =
    if current + 1 >= source.size then '\u0000'
    else source.charAt(current + 1)

  private def `match`(expected: Char): Boolean =
    if isAtEnd() then false
    else if source.charAt(current) != expected then false
    else
      current += 1
      true

  private def string(): Unit =
    while peek() != '"' && !isAtEnd() do
      if peek() == '\n' then line += 1
      advance()

    if isAtEnd() then Lox.error(line, s"Unterminated string")
    else
      advance() // '"'
      val value = source.substring(start + 1, current - 1)
      addToken(TokenType.String, Some(value))

  private def number(): Unit =
    while isDigit(peek()) do advance()
    if peek() == '.' && isDigit(peekNext()) then
      advance() // '.'
      while isDigit(peek()) do advance()
    val value = source.subSequence(start, current).toString().toDouble
    addToken(TokenType.Number, Some(value))

  private def identifier(): Unit =
    while isAlphaNumeric(peek()) do advance()
    val text = source.subSequence(start, current).toString()
    val tp = Keywords.getOrElse(text, TokenType.Identifier)
    addToken(tp)

  private def isDigit(c: Char): Boolean =
    '0' <= c && c <= '9'

  private def isAlpha(c: Char): Boolean =
    ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_'

  private def isAlphaNumeric(c: Char): Boolean =
    isAlpha(c) || isDigit(c)

object Scanner:

  final val Keywords: Map[String, TokenType] = Map(
    "and" -> TokenType.And,
    "class" -> TokenType.Class,
    "else" -> TokenType.Else,
    "false" -> TokenType.False,
    "for" -> TokenType.For,
    "fun" -> TokenType.Fun,
    "if" -> TokenType.If,
    "nil" -> TokenType.Nil,
    "or" -> TokenType.Or,
    "print" -> TokenType.Print,
    "return" -> TokenType.Return,
    "super" -> TokenType.Super,
    "this" -> TokenType.This,
    "true" -> TokenType.True,
    "var" -> TokenType.Var,
    "while" -> TokenType.While,
  )
