package jlox

object Global:

  final val clock: LoxDataType = LoxDataType.Function(new LoxCallable {
    override def arity(): Int = 0

    override def call(interpreter: Interpreter, arguments: Seq[LoxDataType]): LoxDataType =
      LoxDataType.Number(System.currentTimeMillis().toDouble / 1000.0)

    override def toString(): String = "<native fn>"
  })
