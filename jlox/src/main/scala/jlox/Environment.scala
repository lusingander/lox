package jlox

import scala.collection.mutable

class Environment(
    private val enclosing: Option[Environment] = None,
):
  private val values = mutable.Map.empty[String, LoxDataType]

  def define(name: String, value: LoxDataType): Unit =
    values.put(name, value)

  def get(name: Token): LoxDataType =
    values.get(name.lexeme) match
      case Some(v) => v
      case None =>
        enclosing match
          case Some(env) => env.get(name)
          case None      => throw RuntimeError(name, s"Undefined variable '${name.lexeme}'.")

  def getAt(distance: Int, name: String): LoxDataType =
    ancestor(distance).values.get(name).get

  def assign(name: Token, value: LoxDataType): Unit =
    if values.contains(name.lexeme) then values.put(name.lexeme, value)
    else
      enclosing match
        case Some(env) => env.assign(name, value)
        case None      => throw RuntimeError(name, s"Undefined variable '${name.lexeme}'")

  def assignAt(distance: Int, name: Token, value: LoxDataType): Unit =
    ancestor(distance).values.put(name.lexeme, value)

  def ancestor(distance: Int): Environment =
    var env = this
    (0 until distance).foreach: _ =>
      env = env.enclosing.get
    env

  private def debugPrint(): Unit =
    println("[environment]")
    values.foreach: (k, v) =>
      println(s"$k => $v")
    enclosing match
      case Some(e) => e.debugPrint()
      case None    => println("--------------------")
