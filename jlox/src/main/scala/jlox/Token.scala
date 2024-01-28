package jlox

case class Token(
    tp: TokenType,
    lexeme: String,
    literal: Any,
    line: Int,
):
  override def toString(): String = s"$tp $lexeme $literal"

enum TokenType:
  // 記号 1 個のトークン
  case LeftParen
  case RightParen
  case LeftBrace
  case RightBrace
  case Comma
  case Dot
  case Minus
  case Plus
  case Semicolon
  case Slash
  case Star

  // 記号 1 個または 2 個によるトークン
  case Bang
  case BangEqual
  case Equal
  case EqualEqual
  case Greater
  case GreaterEqual
  case Less
  case LessEqual

  // リテラル
  case Identifier
  case String
  case Number

  // キーワード
  case And
  case Class
  case Else
  case False
  case Fun
  case For
  case If
  case Nil
  case Or
  case Print
  case Return
  case Super
  case This
  case True
  case Var
  case While

  // EOF
  case Eof
