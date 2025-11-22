package my.practice.ParserCombinators

import scalaz._
import scala.util.parsing.combinator._
// import std.list._ // type class instances for List
// import syntax.bind._ // syntax for the Bind type class (and its parents)

case class Fix[F[_]](
    unfix: F[Fix[F]]
)
case class Cofree[F[_], A](
    head: A,
    tail: F[Cofree[F, A]]
)
case class Free[F[_], A](
    resume: A \/ F[Free[F, A]]
)

case class CofreeF[F[_], A, B](
    head: A,
    tail: F[B]
)
case class FreeF[F[_], A, B](
    resume: A \/ F[B]
)

// type Cofree[F[_], A] = Fix[CofreeF[F, A, ?]]
// type Free[F[_], A] = Fix[FreeF[F, A, ?]]

// Data Class
case class ProfF[A](
    name: String,
    year: Int,
    students: List[A]
)

// type Prof = Fix[ProfF]
type IdProf = Cofree[ProfF, Int]

// case class Prof(
//     name: String,
//     year: Int,
//     students: List[Prof]
// )

object ProfessorParser extends RegexParsers:
  override def skipWhitespace = false

  def prof(n: Int): Parser[Fix[ProfF]] =
    /*
      indent <- repN(n, elem(" "))
      name <- rep1(elem("non-comma", _ != ","))
      comma <- elem(',')
     */
    def indentParser: Parser[String]     = s"""^ {$n}""".r ^^ { _.toString() }
    // nameParser composed of chars, spaces, round brackets and dots
    def nameParser: Parser[String]       = """[A-Za-z .()]+""".r ^^ { _.toString() }
    def commaSpaceParser: Parser[String] = ", " ^^ { _.toString() }
    def yearParser: Parser[Int]          = """^\d{4}""".r ^^ { _.toInt }
    def newLineParser: Parser[String]    = "\n" ^^ { _.toString() }

    for {
      _    <- indentParser
      name <- nameParser
      _     = println(s"name: $name")
      _    <- commaSpaceParser
      year <- yearParser
      _    <- newLineParser
      ss   <- rep(prof(n + 2))
    } yield Fix(ProfF(name, year, ss))

@main
def ParserCombinators(): Unit =
  val p: IdProf = Cofree(
    1,
    ProfF(
      "Hilbert",
      1885,
      List(
        Cofree(2, ProfF("Ackermann", 1925, Nil)),
        Cofree(3, ProfF("Curry", 1930, Nil)),
        Cofree(
          4,
          ProfF(
            "Weyl",
            1908,
            List(
              Cofree(
                5,
                ProfF(
                  "Mac Lane",
                  1934,
                  List(
                    Cofree(6, ProfF("Howard", 1956, Nil)),
                    Cofree(7, ProfF("Awodey", 1997, Nil))
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  val profParser = ProfessorParser.prof(0)
  val prof       = ProfessorParser.parse(profParser, profsData)

  prof match
    case ProfessorParser.Success(result, _) => pprint.pprintln(result)
    case ProfessorParser.Error(msg, next)   => println(s"ERROR: $msg")
    case ProfessorParser.Failure(msg, _)    => println(s"FAILURE: $msg")

val profsData =
  """Simeon Denis Poisson, 1800
  Gustav Peter Lejeune Dirichlet, 1827
    Rudolf Otto Sigismund Lipschitz, 1853
      C. Felix (Christian) Klein, 1868
        William Edward Story, 1875
          Solomon Lefschetz, 1911
            Albert William Tucker, 1932
              Marvin Lee Minsky, 1954
                Gerald Jay Sussman, 1973
                  Guy Lewis Steele, 1980
                    Philip Lee Wadler, 1984
        C. L. Ferdinand (Carl Louis) Lindemann, 1873
          David Hilbert, 1885
            Wilhelm Ackermann, 1925
            Haskell Curry, 1930
            Hermann Weyl, 1908
              Saunders Mac Lane, 1934
                Steven Awodey, 1997
                William Howard, 1956
""".stripMargin

val profsData2 =
  """1800: Simeon Denis Poisson
  1827: Gustav Peter Lejeune Dirichlet
    1853: Rudolf Otto Sigismund Lipschitz
      1868: C. Felix (Christian) Klein
        1873: C. L. Ferdinand (Carl Louis) Lindemann
          1885: David Hilbert
                1997: Steven Awodey
                1956: William Howard
""".stripMargin
