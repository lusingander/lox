package jlox

import scala.collection.mutable

class LoxInstance(
    private val cls: LoxClass,
):

  private val fields: mutable.Map[String, LoxDataType] = mutable.Map.empty

  def get(name: Token): LoxDataType =
    fields.get(name.lexeme) match
      case Some(f) => f
      case None =>
        cls.findMethod(name.lexeme) match
          case Some(m) => LoxDataType.Function(m.bind(this))
          case None    => throw RuntimeError(name, s"Undefined property '${name.lexeme}'.")

  def set(name: Token, value: LoxDataType): Unit =
    fields.put(name.lexeme, value)

  override def toString(): String = s"${cls.name} instance"
