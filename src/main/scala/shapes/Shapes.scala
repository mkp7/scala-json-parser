// src/main/scala/Shapes.scala
package shapes

case class Point(x: Double = 0.0, y: Double = 0.0):
  def shift(deltaX: Double = 0.0, deltaY: Double = 0.0) =
    copy(x = x + deltaX, y = y + deltaY)

abstract class Shape():
  def draw(offset: Point = Point())(f: String => Unit): Unit =
    f(s"draw: offset = $offset, shape = $this")
  // def draw(f: String => Unit): Unit = f(s"draw: ${this.toString}")

case class Circle(center: Point, radius: Double) extends Shape

case class Rectangle(lowerLeft: Point, height: Double, width: Double)
    extends Shape

case class Triangle(point1: Point, point2: Point, point3: Point) extends Shape

val rec = Rectangle(Point(y = 2.0, x = 1.0), 2.3, 4.0)

