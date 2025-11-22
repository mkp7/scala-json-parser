// src/main/scala/Futures.scala
package scalaJsonParser.futures

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}
import scala.annotation.tailrec
import scala.annotation.targetName

val i: Int = 123 // decimal
val x: Long = 0x123L // hexadecimal
val f: Float = 123_456.789f // float
val d: Double = 123_456_789.0123 // double
val y: BigInt = 0x123_a4b // big int
val z: BigDecimal = 123_456_789.01234 // big decimal

case class Money(value: BigDecimal)
case object Money:
  def apply(s: String): Money = apply(BigDecimal(s.toDouble))
  def apply(d: Double): Money = apply(BigDecimal(d))

object Mean1:
  @targetName("applyOne")
  def apply(ds: Double*): Double = apply(ds)
  def apply(d: Double, ds: Double*): Double = apply(ds :+ d)
  def apply(ds: Seq[Double]): Double = ds.sum / ds.size

  def calc2a(ds: Double*): Double = ds.sum / ds.size
  def calc2b(ds: Seq[Double]): Double = calc2a(ds*)

def sleep(ms: Long) = Thread.sleep(ms)

def factorial(n: Int): BigInt =
  @tailrec
  def fact(i: Int, acc: BigInt): BigInt =
    if i <= 1 then acc
    else fact(i - 1, i * acc)

  fact(n, 1)

given MyImplicit: String = "DefaultValue"

def imFn()(using value: String): String = value

@main
def Futures(): Unit =

  (1 to 200).foreach { i =>

    val future = Future {
      val duration = (math.random * 50).toLong
      sleep(duration)
      if i == 3 then throw RuntimeException(s"$i -> $duration")
      duration
    }(using global)

    future.onComplete {
      case Success(value)     => println(s"Success! #$i -> $value")
      case Failure(exception) => println(s"FAILURE! #$i -> $exception")
    }
  }

  sleep(1200)
  println("future work finished!")

  val fact10 = factorial(10)
  println(s"fact10: $fact10")

  val myUsingResultOne = imFn()
  println(s"myUsingResultOne: $myUsingResultOne")

  val myUsingResultTwo = {
    given MyAnotherImplicit: String = "AnotherDefaultValue"

    imFn()
  }
  println(s"myUsingResultTwo: $myUsingResultTwo")

  val myUsingResultThree = imFn()(using value = "MyValue")
  println(s"myUsingResultThree: $myUsingResultThree")

  val stateCapitals = Map(
    "Gujarat" -> "Gandhinagar",
    "Maharashtra" -> "Mumbai",
    "Karnataka" -> "Bangalore"
  )
