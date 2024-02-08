package jlox

import java.nio.file.Files
import java.nio.file.Paths
import java.nio.charset.Charset
import scala.io.StdIn

def main(args: Array[String]): Unit =
  if args.length > 1 then
    println("Usage: jlox [script]")
    System.exit(64)
  else if args.length == 1 then
    println("runFile:")
    Lox.runFile(args(0))
  else
    println("runPromt:")
    Lox.runPrompt()

object Lox:
  var hadError = false
  var hadRuntimeError = false

  val interpreter = Interpreter()

  def runFile(path: String): Unit =
    val bytes = Files.readAllBytes(Paths.get(path))
    run(String(bytes, Charset.defaultCharset()))

    if (hadError) then System.exit(65)
    if (hadRuntimeError) then System.exit(70)

  def runPrompt(): Unit =
    while true do
      print("> ")
      val line = StdIn.readLine()
      if line == null then return
      else
        run(line)
        hadError = false

  private def run(source: String): Unit =
    val scanner = Scanner(source)
    val tokens = scanner.scanTokens()

    val parser = Parser(tokens)
    val statements = parser.parse()

    if hadError then return

    val resolver = Resolver(interpreter)
    resolver.resolve(statements)

    if hadError then return

    interpreter.interpret(statements)

  def error(line: Int, message: String): Unit =
    report(line, "", message)

  def error(token: Token, message: String): Unit =
    if token.tp == TokenType.Eof then report(token.line, " at end", message)
    else report(token.line, s" at '${token.lexeme}'", message)

  def runtimeError(e: RuntimeError): Unit =
    Console.err.println(s"${e.getMessage()}\n[line ${e.token.line}]")
    hadRuntimeError = true

  def report(line: Int, where: String, message: String): Unit =
    Console.err.println(s"[line $line] Error$where: $message")
    hadError = true
