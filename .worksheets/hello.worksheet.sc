// val jsonString = """{"primes": [2, 3, 5], "evens": [2, 4, 6]}"""
// parseNull(jsonString)

val myVal = List(1, "abc", 3)

// def parseValue[T](inputString: String): T =
//   var data: T
//   data match
//     case (data: String) => println("abc")
//     case (data: Double) => println(123)
//     case _: T           => println("not supported")

// val jsonString =
//   """{"primes": [2, 3, 5], "evens": [2, 4, 6], "nullKey": null}"""
// val map: Map[String, List[Int]] =
//   upickle.default.read[Map[String, List[Int] | Null]](jsonString)
// println(map)

def factorial(n: Int): Int =
  n match
    case 1 => 1
    // case _: Int => (x: Int) => x *
    case _ => n * factorial(n - 1)

factorial(50)

// val ies = 1 to 3
val ijs = for {
  i <- 1 to 6
  if i <= 3
  j <- 'a' to 'c'
  if (j == 'b' || j == 'c')
} yield (i, j)

ijs

val irange = 1 to 5

irange.map

for {
  i <- 1 to 5
  j <- 6 to 10
} yield (i, j)

// val ies = for { i <- 1 to 5 } yield i

// ies
