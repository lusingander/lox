package jlox

import scala.collection.mutable

class Environment:
  private val values = mutable.Map.empty[String, LoxDataType]

  def define(name: String, value: LoxDataType): Unit =
    values.put(name, value)

  def get(name: Token): LoxDataType =
    values.getOrElse(name.lexeme, throw RuntimeError(name, s"Undefined variable '${name.lexeme}'."))

  def assign(name: Token, value: LoxDataType): Unit =
    if values.contains(name.lexeme) then values.put(name.lexeme, value)
    else throw RuntimeError(name, s"Undefined variable '${name.lexeme}'")
