package jlox

class LoxInstance(
    private val cls: LoxClass,
):

  override def toString(): String = s"${cls.name} instance"
