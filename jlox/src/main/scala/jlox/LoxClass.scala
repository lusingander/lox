package jlox

class LoxClass(
    val name: String,
    val methods: Map[String, LoxFunction],
) extends LoxCallable:

  override def arity(): Int = 0

  override def call(interpreter: Interpreter, arguments: Seq[LoxDataType]): LoxDataType =
    val instance = LoxInstance(this)
    LoxDataType.Instance(instance)

  def findMethod(name: String): Option[LoxFunction] =
    methods.get(name)

  override def toString(): String = name
