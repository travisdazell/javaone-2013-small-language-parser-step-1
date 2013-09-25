package net.travisdazell.parsers

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.collection.mutable.HashMap

// simple monolithic scope program with variable statements
// 		variables are defined using the "var" keyword
// 		values are printed with the "println" statement
// 		if you reference an undefined variable, you'll get an error
class SmallLanguageParser extends StandardTokenParsers {
  val vars = new HashMap[String, Int]

  lexical.reserved += ("var", "println")
  lexical.delimiters += ("*", "/", "%", "+", "-", "(", ")", "=")

  def program: Parser[Any] = rep(variableAssignment | outStatement)

  def variableAssignment: Parser[Any] = ("var" ~> ident <~ "=") ~ expr ^^ { 
    case a ~ b => vars(a) = b
  }

  def outStatement: Parser[Any] = "println" ~> expr ^^ { e => println(e) }

  def expr: Parser[Int] = term ~ rep(("+" | "-") ~ term ^^ {
    case "+" ~ t => t
    case "-" ~ t => -t
  }) ^^ { case t ~ r => t + r.sum }

  def term: Parser[Int] = multiplydividemodulo | factor ^^ { _.toInt }

  def multiplydividemodulo: Parser[Int] = factor ~
		  rep(("*" | "/" | "%") ~ factor) ^^ {
    case a ~ List() => a
    case a ~ b => {
      var result = a

      for (f <- b) {
        result =
          f._1 match {
            case "*" => result * f._2
            case "/" => result / f._2
            case "%" => result % f._2
          }
      }

      result
    }
  }

  def factor: Parser[Int] = numericLit ^^ { _.toInt } |
    "(" ~> expr <~ ")" ^^ { e => e } |
    ident ^^ {
      a =>
        {
          if (vars.contains(a)) vars.get(a).get
          else {
            sys.error("Undefined variable " + a + " at position [" +
                lastNoSuccess.next.pos.column + "] on line: " +
                lastNoSuccess.next.pos.line)
          }
        }
    }

  def parseAll[T](p: Parser[T], in: String): ParseResult[T] = {
    val input = new lexical.Scanner(in)

    val result: ParseResult[T] =
      try {
        phrase(p)(input)
      } catch {
        case e: RuntimeException => this.Error(e.getMessage, input)
      }

    result
  }
}