package jlox

case class Token(
    tp: TokenType,
    lexeme: String,
    literal: Any,
    line: Int,
):
  override def toString(): String = s"$tp $lexeme $literal"

enum TokenType:
  case
    // 記号 1 個のトークン
    LeftParen, RightParen, LeftBrace, RightBrace, Comma, Dot, Minus,
    Plus, Semicolon, Slash, Star,
    // 記号 1 個または 2 個によるトークン
    Bang, BangEqual, Equal, EqualEqual, Greater, GreaterEqual, Less,
    LessEqual,
    // リテラル
    Identifier, String, Number,
    // キーワード
    And, Class, Else, False, Fun, For, If, Nil, Or, Print, Return, Super, This,
    True, Var, While,
    // EOF
    Eof
