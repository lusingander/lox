package jlox

class LoxFunction(
    private val declaration: Stmt.Function,
) extends LoxCallable:

  override def arity(): Int = declaration.params.size

  override def call(interpreter: Interpreter, arguments: Seq[LoxDataType]): LoxDataType =
    val environment = Environment(Some(interpreter.global))
    declaration.params
      .zip(arguments)
      .foreach: (param, argument) =>
        environment.define(param.lexeme, argument)

    interpreter.executeBlock(declaration.body)(using environment)
    LoxDataType.Nil

  override def toString(): String = s"<fn ${declaration.name.lexeme}>"
