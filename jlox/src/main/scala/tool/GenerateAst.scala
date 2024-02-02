package tool

import java.io.PrintWriter
import java.nio.charset.StandardCharsets

// sbt run ./src/main/scala/jlox/
def main(args: Array[String]): Unit =
  GenerateAst().run(args)

class GenerateAst:
  def run(args: Array[String]): Unit =
    if args.size != 1 then
      Console.err.println("Usage: generate_ast <output directory>")
      System.exit(64)

    val outputDir = args(0)
    val types = Seq(
      ("Binary", "left: Expr, operator: Token, right: Expr"),
      ("Grouping", "expression: Expr"),
      ("Literal", "value: LoxDataType"),
      ("Unary", "operator: Token, right: Expr"),
    )
    defineAst(outputDir, "Expr", types)

  // enum に個別のメソッド定義できない & ひとまず実装をサンプルに寄せるために sealed trait で定義
  private def defineAst(
      outputDir: String,
      baseName: String,
      types: Seq[(String, String)],
  ): Unit =
    val path = s"$outputDir/$baseName.scala"
    val writer = PrintWriter(path, StandardCharsets.UTF_8)

    writer.println("package jlox")
    writer.println()
    writer.println(s"sealed trait $baseName:")
    writer.println(s"  def accept[R](visitor: $baseName.Visitor[R]): R")
    writer.println()
    writer.println(s"object $baseName:")

    types.foreach: t =>
      val caseName = t._1
      val fields = t._2
      writer.println(s"  case class $caseName($fields) extends $baseName:")
      writer.println(s"    override def accept[R](visitor: Visitor[R]): R =")
      writer.println(s"      visitor.visit$caseName$baseName(this)")
      writer.println()

    writer.println("  trait Visitor[R]:")
    types.foreach: t =>
      val caseName = t._1
      writer.println(
        s"    def visit$caseName$baseName(${baseName.toLowerCase()}: $baseName.$caseName): R",
      )

    writer.close()
