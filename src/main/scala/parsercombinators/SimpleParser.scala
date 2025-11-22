package parsercombinators

import scala.util.parsing.combinator._

class SimpleParser extends RegexParsers {
  def word: Parser[String] = """[a-z]+""".r ^^ { _.toString }
}

object TestSimpleParser extends SimpleParser {
  def main(args: Array[String]) = {
    parse(word, "johnny come lately") match {
      case Success(matched, _) => println(matched)
      case Failure(msg, _)     => println("FAILURE: " + msg)
      case Error(msg, _)       => println("ERROR: " + msg)
    }
  }
}

