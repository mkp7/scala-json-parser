package jsonparser

import scala.util.matching.Regex

// type Json = null, boolean, number, string, array, object

case class JsonParserValue[T](
    value: T,
    remString: String
)

def parseNull(jsonString: String): Option[JsonParserValue[Null]] =
  jsonString match
    case s"null$remString" => Some(JsonParserValue(null, remString))
    case _                 => None

def parseBool(jsonString: String): Option[JsonParserValue[Boolean]] =
  jsonString match
    case s"true$remString"  => Some(JsonParserValue(true, remString))
    case s"false$remString" => Some(JsonParserValue(false, remString))
    case _                  => None

val numberPattern =
  """^(-?(0|([1-9]\d*))(\.\d+)?((e|E)(\+|-)?\d+)?)(.*)""".r.unanchored
def parseNumber(jsonString: String): Option[JsonParserValue[Double]] =
  jsonString match
    case numberPattern(jsonNumber, remMatches*) =>
      Some(JsonParserValue(jsonNumber.toDouble, remMatches.last))
    case _ => None

val valueParsers = List(parseNull, parseBool, parseNumber)
// def parseValue(jsonString: String): Any =

@main def main(): Unit =
  val jsonString =
    """{"primes": [2, 3, 5], "evens": [2, 4, 6], "nullKey": null}"""
  val map: Map[String, List[Int]] =
    upickle.default.read[Map[String, List[Int] | Null]](jsonString)
  println(map)

  val jsonNullString = """null123abc"""
  val jsonNullData = parseNull(jsonNullString)
  jsonNullData match
    case Some(value) => println(value)
    case None        => println("no value")

  val jsonBoolString = """false123abc"""
  val jsonBoolData = parseBool(jsonBoolString)
  jsonBoolData match
    case Some(value) => println(value)
    case None        => println("no value")

  val jsonNumberString = """-123.2E-4abcnull"""
  val jsonNumberData = parseNumber(jsonNumberString)
  jsonNumberData match
    case Some(value) => println(value)
    case None        => println("no value")

