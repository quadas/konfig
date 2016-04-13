package konfig

import scalaprops._
import scalaz.{ @@, Tag }
import konfig.Konfig._
import com.typesafe.config.{ ConfigFactory, Config }

import scala.language.implicitConversions

package model {
  sealed trait A
  case class A1(a: Int) extends A
  case class A2(a: Long) extends A
  case class A3(a: String) extends A

  sealed trait B
  case class B1(a: A) extends B
  case class B2(b: Int) extends B

  sealed trait C
  case object C1 extends C
  case class C2(a: A) extends C

  case class D(a: A, b: B, c: List[C])
}

object KonfigTest extends Scalaprops {
  import konfig.model._

  type Str = String @@ GenTags.AlphaNum
  implicit def strToStrign(s: Str): String = Tag.unwrap(s)

  def parseConfig(c: String): Config = ConfigFactory.parseString(c)

  val basic = Property.forAll {
    (a: Int, b: Int, c: Int, d: Int, e: Int) =>
      parseConfig(
        s"""
              d {
                a {
                  type = A1
                  a = $a
                }
                b {
                  type = B1
                  a {
                    type = A2
                    a = $b
                  }
                }
                c = [
                  {
                    type = C1
                  },
                  {
                    type = C2
                    a = {
                      type = A3
                      a = $c
                    }
                  }
                ]
              }
          """
      ).read[D]("d") == D(A1(a), B1(A2(b)), List(C1, C2(A3(c.toString))))
  }
}