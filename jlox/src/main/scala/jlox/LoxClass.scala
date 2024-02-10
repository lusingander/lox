package jlox

class LoxClass(
    val name: String,
    val methods: Map[String, LoxFunction],
) extends LoxCallable:

  override def arity(): Int =
    val initializer = findMethod("init")
    initializer match
      case Some(init) => init.arity()
      case None       => 0

  override def call(interpreter: Interpreter, arguments: Seq[LoxDataType]): LoxDataType =
    val instance = LoxInstance(this)
    val initializer = findMethod("init")
    initializer.foreach: init =>
      init.bind(instance).call(interpreter, arguments)
    LoxDataType.Instance(instance)

  def findMethod(name: String): Option[LoxFunction] =
    methods.get(name)

  override def toString(): String = name
