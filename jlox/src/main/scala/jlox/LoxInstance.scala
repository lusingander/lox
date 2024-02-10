package jlox

import scala.collection.mutable

class LoxInstance(
    private val cls: LoxClass,
):

  private val fields: mutable.Map[String, LoxDataType] = mutable.Map.empty

  def get(name: Token): LoxDataType =
    fields.getOrElse(name.lexeme, throw RuntimeError(name, s"Undefined property '${name.lexeme}'."))

  def set(name: Token, value: LoxDataType): Unit =
    fields.put(name.lexeme, value)

  override def toString(): String = s"${cls.name} instance"
