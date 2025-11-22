// src/main/scala/ProcessMessages.scala
package scalaJsonParser.shapes

object ProcessMessages:
  def apply(message: Message): Message =
    message match
      case Exit =>
        println(s"ProcessMessage Exit: exiting...")
        Exit
      case Draw(shape) =>
        shape.draw()(str => println(s"ProcessMessage Draw: $str"))
        Response(s"ProcessMessage: $shape drawn")
      case Response(unexpected) =>
        val response = Response(s"ERROR: Unexpected Response: $unexpected")
        println(s"ProcessMessage Response: $response")
        response
    end match
  end apply
end ProcessMessages
