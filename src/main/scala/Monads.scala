package my.practice.monads

import my.practice.monads.OptionFunctor.map

/*
==Monoid==
Properties:
    - Identity: *no-op* value (it doesn't do anything when used with `append` method)
    - Associativity: grouping doesn't matter (grouping of the `append` methods isn't going to matter)
 */
trait Monoid[A]:
  def identity: A
  def append(a: A, b: A): A

object StringConcat extends Monoid[String]:
  def identity: String                     = ""
  def append(a: String, b: String): String = a + b

object IntAddition extends Monoid[Int]:
  def identity: Int               = 0
  def append(a: Int, b: Int): Int = a + b

object IntMultiplication extends Monoid[Int]:
  def identity: Int               = 1
  def append(a: Int, b: Int): Int = a * a

object FunctionComposition /* extends Monoid[? => ?] */:
  def identity[A]: A => A                             = (a: A) => a
  def append[A, B, C](f1: A => B, f2: B => C): A => C =
    (a: A) => (f1 andThen f2)(a)

/*
==Functor==
Properties:
    - Identity: *no-op* value (it doesn't do anything when used with `map` method)
    - Composition: grouping doesn't matter (grouping of the `map` methods isn't going to matter)
 */
trait Functor[F[_]]:
  def map[A, B](a: F[A])(fn: A => B): F[B]

sealed trait Option[+A]
case class Some[A](a: A) extends Option[A]
case object None         extends Option[Nothing]

object OptionFunctor extends Functor[Option]:
  def map[A, B](a: Option[A])(fn: A => B): Option[B] =
    a match
      case Some(value) => Some(fn(value))
      case None        => None

sealed trait Add[+A]
case class AddBox[A](a: A) extends Add[A]
// case class IntBox(a: Int) extends Add[Int]

object AddFunctor extends Functor[Add]:
  def map[A, B](a: Add[A])(fn: A => B): Add[B] =
    a match
      case AddBox(a) => AddBox(fn(a))

  // def map(a: Add[String])(fn: String => Int): IntBox =
  //   // IntBox(fn(a.a))
  //   a match
  //     case StringBox(value) => IntBox(fn(value))

/*
==Monad==
Properties:
    - Identity: *no-op* value (it doesn't do anything when used with `map` method)
    - Composition: grouping doesn't matter (grouping of the `map` methods isn't going to matter)
 */
trait Monad[M[_]] extends Functor[M]:
  def pure[A](a: A): M[A]
  def flatMap[A, B](a: M[A])(fn: A => M[B]): M[B]
  def map[A, B](a: M[A])(fn: A => B): M[B]                     =
    flatMap(a)((b: A) => pure(fn(b)))
  def append[A, B, C](f1: A => M[B], f2: B => M[C]): A => M[C] =
    (a: A) => flatMap(f1(a))(f2)

object OptionMonad extends Monad[Option]:
  def pure[A](a: A): Option[A] = Some(a)

  def flatMap[A, B](a: Option[A])(fn: A => Option[B]): Option[B] =
    a match
      case Some(value) => fn(value)
      case None        => None

@main
def HelloMonads(): Unit =

  val someVal: None.type = None

  // val strToInt = AddFunctor.map(StringBox("2"))(a => a.toInt)
  val strToInt =
    AddFunctor.map[String, Int](a = AddBox("2"))(a => a.toInt)

  // example one:
  def identity[A](a: A): A = a
  val someValueOne         = map(Some("value"))(identity)
  println(s"someValueOne: $someValueOne")

  // example two:
  val f: String => String = s => s + "a"
  val g: String => String = s => s + "l"
  val h: String => String = s => s + "a"
  val i: String => String = s => s + " world"

  val someValueTwo    = map(Some("sc"))(f andThen g andThen h andThen i)
  println(s"someValueTwo: $someValueTwo")
  val someValueTwoAlt = map(map(map(map(Some("sc"))(f))(g))(h))(i)
  println(s"someValueTwoAlt: $someValueTwoAlt")

  ()
