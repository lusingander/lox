package jlox

class LoxFunction(
    private val declaration: Stmt.Function,
    private val closure: Environment,
    private val isInitializer: Boolean = false,
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
      if isInitializer then closure.getAt(0, "this")
      else LoxDataType.Nil
    catch
      case e: Interpreter.Return =>
        if isInitializer then closure.getAt(0, "this")
        else e.value

  def bind(instance: LoxInstance): LoxFunction =
    val environment = Environment(Some(closure))
    environment.define("this", LoxDataType.Instance(instance))
    LoxFunction(declaration, environment, isInitializer)

  override def toString(): String = s"<fn ${declaration.name.lexeme}>"
