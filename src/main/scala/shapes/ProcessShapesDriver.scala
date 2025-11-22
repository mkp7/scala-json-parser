// src/main/scala/ProcessShapesDriver.scala
package shapes

val func: Message => String = message =>
  message match
    case Exit              => "Got Exit"
    case Draw(shape)       => s"Got Draw($shape)"
    case Response(message) => s"Got Response($message)"

val pfunc: PartialFunction[Message, String] =
  case Exit              => "Got Exit"
  case Draw(shape)       => s"Got Draw($shape)"
  case Response(message) => s"Got Response($message)"

val pfs: PartialFunction[Matchable, String] =
  case s: String => "String"

val pfd: PartialFunction[Matchable, String] =
  case d: Double => "YES"

val pfsd = pfs.orElse(pfd)

val nfsd = pfsd.lift

def tryPF(x: Matchable, f: PartialFunction[Matchable, String]): String =
  try f(x)
  catch case _: MatchError => "ERROR!"

@main
def ProcessShapesDriver =

  val nfval = nfsd(12)

  nfval match
    case Some(value) => value
    case None        => "no value"

  val messages = Seq(
    Draw(Circle(Point(), 1.0)),
    Draw(Rectangle(Point(), 2, 5)),
    Response(s"Say hellow to pi: 3.14159..."),
    Draw(Triangle(Point(), Point(x = 2), Point(x = 1, y = 2))),
    Exit
  )

  messages.foreach { message =>
    val response = ProcessMessages(message)
    println(response)
  }

