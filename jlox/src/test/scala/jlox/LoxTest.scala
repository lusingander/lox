package jlox

class LoxTest extends LoxTestBase:

  test("==, !="):
    val source =
      """
        |print "";
        |print "foo" == "bar";
        |print "foo" == "f" + "o" + "o";
        |print 2 * 4 == 5 + 3;
        |print nil == nil;
        |print "123" == 123;
        |print 1 != 2;
        |print true != true;
        |
        |""".stripMargin
    val expected =
      """
        |false
        |true
        |true
        |true
        |false
        |true
        |false
        |""".stripMargin
    assertOutput(run(source))(expected)

  test("number +, -, *, /"):
    val source =
      """
        |print "";
        |print 1 + 2 * 3;
        |print 10 - 4 / 2;
        |print (1 + 2) * 3;
        |print 10 - (2 - 3);
        |print 1.23 * 3;
        |print 5 / 2;
        |
        |""".stripMargin
    val expected =
      """
        |7
        |8
        |9
        |11
        |3.69
        |2.5
        |""".stripMargin
    assertOutput(run(source))(expected)

  test("number <, <=, >, >="):
    val source =
      """
        |print "";
        |print 2 + 3 <= 4;
        |print 4 < 4;
        |print 1 + 2 >= 2 + 1;
        |print -5 > -3;
        |
        |""".stripMargin
    val expected =
      """
        |false
        |false
        |true
        |false
        |""".stripMargin
    assertOutput(run(source))(expected)

  test("var, assign, scope"):
    val source =
      """
        |print "";
        |
        |var x = 1;
        |var y;
        |
        |print x;
        |print y;
        |
        |x = "foo";
        |y = "bar";
        |var z = x + y + "baz";
        |var w = "qux";
        |
        |{
        |  var x = true;
        |  var y = false;
        |  w = 1;
        |  print x;
        |  print y;
        |  print z;
        |  print w;
        |  {
        |    var x = nil;
        |    w = 2;
        |    print x;
        |    print y;
        |    print z;
        |    print w;
        |  }
        |}
        |print x;
        |print y;
        |print z;
        |print w;
        |
        |""".stripMargin
    val expected =
      """
        |1
        |nil
        |true
        |false
        |foobarbaz
        |1
        |nil
        |false
        |foobarbaz
        |2
        |foo
        |bar
        |foobarbaz
        |2
        |""".stripMargin
    assertOutput(run(source))(expected)

  test("if, else, and, or"):
    val source =
      """
        |print "";
        |
        |var a = 5;
        |
        |if (a > 3)
        |  print "a";
        |
        |if (a < 3)
        |  print "b";
        |
        |var b;
        |
        |if (b) {
        |  print "c";
        |  print "d";
        |} else {
        |  print "e";
        |  print "f";
        |}
        |
        |if (false) {
        |  print "g";
        |} else if (true) {
        |  print "h";
        |} else {
        |  print "i";
        |}
        |
        |var x;
        |if (0)
        |  if ("")
        |    if (nil)
        |      x = "j";
        |    else
        |      x = "k";
        |  else
        |    x = "l";
        |
        |print x;
        |
        |print false and 100;
        |print nil or 200;
        |print "" and nil;
        |print 0 or true;
        |print nil and nil;
        |
        |print 10 and 20 and 30 and 40;
        |
        |""".stripMargin
    val expected =
      """
        |a
        |e
        |f
        |h
        |k
        |false
        |200
        |nil
        |0
        |nil
        |40
        |""".stripMargin
    assertOutput(run(source))(expected)

  test("while, for"):
    val source =
      """
        |print "";
        |
        |var a = 1;
        |while (a < 5) {
        |  print a;
        |  a = a + 1;
        |}
        |
        |for (var b = 0; b < 3; b = b + 1)
        |  print b;
        |
        |var c;
        |for ( ; !c; ) {
        |  print c;
        |  c = "end";
        |}
        |""".stripMargin
    val expected =
      """
        |1
        |2
        |3
        |4
        |0
        |1
        |2
        |nil
        |""".stripMargin
    assertOutput(run(source))(expected)

  private def run(source: String): Unit =
    val tokens = Scanner(source).scanTokens()
    val statements = Parser(tokens).parse()
    Interpreter().interpret(statements)