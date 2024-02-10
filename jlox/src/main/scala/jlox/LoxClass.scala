package jlox

class LoxClass(
    val name: String,
) extends LoxCallable:

  override def arity(): Int = 0

  override def call(interpreter: Interpreter, arguments: Seq[LoxDataType]): LoxDataType =
    val instance = LoxInstance(this)
    LoxDataType.Instance(instance)

  override def toString(): String = name
