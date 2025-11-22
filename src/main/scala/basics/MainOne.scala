// src/main/scala/scalaJsonParser/introScala/MainOne.scala
package basics

object MainOne:
  def main(params: Array[String]): Unit =
    print("object MainOne.main: ")
    params
      .map(s => s.toUpperCase)
      .foreach(s => printf("%s ", s))
    println("")

def main(params: Array[String]): Unit =
  print("top level main function: ")
  params
    .map(s => s.toUpperCase)
    .foreach(s => printf("%s ", s))
  println("")

@main def Hello(params: String*): Unit =
  print("top level Hello function with @main annotation: ")
  val output = params
    .map(_.toUpperCase)
    .mkString("[", " ^ ", "]")
  println(output)

