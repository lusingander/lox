package jlox

import java.lang.{String as ScalaString}
import scala.{Double as ScalaDouble, Boolean as ScalaBoolean}

enum LoxDataType:
  case Number(value: ScalaDouble)
  case String(value: ScalaString)
  case Bool(value: ScalaBoolean)
  case Nil
  case Function(value: LoxCallable)

  override def toString(): ScalaString =
    this match
      case LoxDataType.Number(v) =>
        val text = v.toString()
        if text.endsWith(".0") then text.substring(0, text.size - 2)
        else text
      case LoxDataType.String(v)   => v.toString()
      case LoxDataType.Bool(v)     => v.toString()
      case LoxDataType.Nil         => "nil"
      case LoxDataType.Function(v) => v.toString()
