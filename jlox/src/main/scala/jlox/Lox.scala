package jlox

import java.nio.file.Files
import java.nio.file.Paths
import java.nio.charset.Charset
import java.io.InputStreamReader
import java.io.BufferedReader
import scala.annotation.tailrec

var hadError = false

def main(args: Array[String]): Unit =
  if args.length > 1 then
    println("Usage: jlox [script]")
    System.exit(64)
  else if args.length == 1 then
    println("runFile:")
    runFile(args(0))
  else
    println("runPromt:")
    runPrompt()

def runFile(path: String): Unit =
  val bytes = Files.readAllBytes(Paths.get(path))
  run(new String(bytes, Charset.defaultCharset()))

  if (hadError) then System.exit(65)

def runPrompt(): Unit =
  val input = new InputStreamReader(System.in)
  val reader = new BufferedReader(input)

  while true do
    print("> ")
    val line = reader.readLine()
    if line == null then return
    else
      run(line)
      hadError = false

def run(source: String): Unit =
  val scanner = new Scanner(source)
  val tokens = scanner.scanTokens()

  tokens.foreach { token =>
    println(token)
  }

def error(line: Int, message: String): Unit =
  report(line, "", message)

def report(line: Int, where: String, message: String): Unit =
  Console.err.println(s"[line $line] Error$where: $message")
  hadError = true
