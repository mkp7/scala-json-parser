package basics

import scala.math.*
import scala.io.Source
import io.Source.*
import collection.immutable.{List, Map}
import scala.collection.immutable.Vector
import collection.immutable.Vector

package optionTests:
  sealed abstract class OptionClass[+A]
  case class Some[+A](a: A) extends OptionClass[A]
  case object None extends OptionClass[Nothing]

  enum OptionEnum[+A] {
    case Some(a: A)
    case None
  }

package typesTests:
  val mapOne = Map(("one", 1), ("two", 2))

  val fnMap: (Map[String, String | Int]) => String = (mapOne) => "mapOne"

  val fnVal = fnMap(mapOne)

package nullsTests:
  import java.util.HashMap as JHashMap
  import scala.collection.immutable.{BitSet as _, LazyList, HashMap as HMap}

  @main
  def Nulls() =
    val jhm = JHashMap[String, String]()
    jhm.put("1", "One")

    val One1: String = jhm.get("1")
    val Two2: String | Null = jhm.get("2")

    val testOption =
      optionTests.OptionEnum.Some("str")

