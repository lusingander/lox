package jlox

trait LoxCallable:
  def arity(): Int
  def call(interpreter: Interpreter, arguments: Seq[LoxDataType]): LoxDataType
