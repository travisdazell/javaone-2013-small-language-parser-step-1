package net.travisdazell.parsers

import scala.io.Source

object SmallLanguageParserTester {
  def main(args: Array[String]) {
    val inputFile = Source.fromFile("scripts/program.small")
    val inputSource = inputFile.mkString

    val parser = new SmallLanguageParser
   	parser.parseAll(parser.program, inputSource) match {
      case parser.Error(msg, n) => println("Error: " + msg)
      case _ =>
    }
  }
}