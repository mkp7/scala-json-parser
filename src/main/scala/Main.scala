// type Json = null, boolean, number, string, array, object

case class JsonParserValue[T](
    value: T,
    remString: String
)

def parseNull(jsonString: String): Option[JsonParserValue[Null]] =
  jsonString match
    case s"null$remString" => Some(JsonParserValue(null, remString))
    case _                 => None

@main def main(): Unit =
  val jsonString = """{"primes": [2, 3, 5], "evens": [2, 4, 6]}"""
  val map: Map[String, List[Int]] =
    upickle.default.read[Map[String, List[Int]]](jsonString)
  println(map)

  val jsonNullString = """null123abc"""
  val jsonNullData = parseNull(jsonNullString)
  jsonNullData match
    case Some(value) => println(value)
    case None        => println("no value")
