package jlox

import scala.collection.mutable

class Environment(
    val enclosing: Option[Environment] = None,
):
  private val values = mutable.Map.empty[String, LoxDataType]

  def define(name: String, value: LoxDataType): Unit =
    values.put(name, value)

  def get(name: Token): LoxDataType =
    values.get(name.lexeme) match
      case Some(v) => v
      case None =>
        enclosing match
          case Some(env) => env.get(name)
          case None      => throw RuntimeError(name, s"Undefined variable '${name.lexeme}'.")

  def assign(name: Token, value: LoxDataType): Unit =
    if values.contains(name.lexeme) then values.put(name.lexeme, value)
    else
      enclosing match
        case Some(env) => env.assign(name, value)
        case None      => throw RuntimeError(name, s"Undefined variable '${name.lexeme}'")
