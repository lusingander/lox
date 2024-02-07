package jlox

class LoxFunction(
    private val declaration: Stmt.Function,
    private val closure: Environment,
) extends LoxCallable:

  override def arity(): Int = declaration.params.size

  override def call(interpreter: Interpreter, arguments: Seq[LoxDataType]): LoxDataType =
    val environment = Environment(Some(closure))
    declaration.params
      .zip(arguments)
      .foreach: (param, argument) =>
        environment.define(param.lexeme, argument)

    try
      interpreter.executeBlock(declaration.body)(using environment)
      LoxDataType.Nil
    catch
      case e: Interpreter.Return =>
        e.value

  override def toString(): String = s"<fn ${declaration.name.lexeme}>"
