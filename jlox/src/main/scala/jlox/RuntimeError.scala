package jlox

class RuntimeError(
    val token: Token,
    message: String,
) extends RuntimeException(message)
