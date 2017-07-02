package com.quadas.konfig

import scala.concurrent.duration._

import com.typesafe.config.{ Config, ConfigFactory }
import org.scalacheck._
import org.scalatest.prop.Checkers
import org.scalatest.{ FlatSpec, Matchers }

class KonfigTest extends FlatSpec with Matchers with Checkers {
  sealed trait Database
  case class Mysql(host: String, port: Int) extends Database
  case class Postgres(host: String, port: Int) extends Database
  case class App(
    database: Database,
    ssl: Boolean,
    sslProtocols: List[String],
    listenPort: Option[Int]
  )

  def parseConfig(c: String): Config = ConfigFactory.parseString(c)
  implicit val arbiStr = Arbitrary(Gen.alphaStr)

  "case class derivation" should "work" in {
    check(Prop.forAll {
      (host: String, port: Int, ssl: Boolean, protocols: List[String], listenPort: Int) =>
        val h = host.replaceAll("\"", "")
        val protos = protocols.map(_.replaceAll("\"", ""))
        val app = App(
          Postgres(h, port),
          ssl,
          protos,
          Some(listenPort)
        )
        val protoStr = if (protos.isEmpty) "[]" else protos.mkString("[\"", "\",\"", "\"]")
        val conf = parseConfig(
          s"""
            app {
              database {
                type = "Postgres"
                host = "$h"
                port = $port
              }
              ssl = $ssl
              ssl-protocols = $protoStr
              listen-port = $listenPort
            }
          """
        )
        val parsed = conf.read[App]("app")
        parsed == app
    })
  }

  "hyphen style key conversion" should "work" in {
    check(Prop.forAll {
      (a: String) =>
        KeyStyle.Hyphen.style(a).filter(_.isUpper).isEmpty
    })
  }

}

class HyphenStyleKeySpec extends FlatSpec with Matchers {
  "Hyphen style conversion" should "work" in {
    KeyStyle.Hyphen.style("someKey") should be("some-key")
    KeyStyle.Hyphen.style("some_other_key") should be("some_other_key")
    KeyStyle.Hyphen.style("someDigits123123") should be("some-digits123123")
    KeyStyle.Hyphen.style("AAAAA") should be("a-a-a-a-a")
  }
}

class KeyStyleCustomizableSpec extends FlatSpec with Matchers {
  case class T(ABCDEF: String)
  implicit val keyStyle = KeyStyle.Same
  "Keystyle" should "be customizable" in {
    ConfigFactory.parseString("t { ABCDEF = \"aaaa\" }")
      .read[T]("t") should be(T("aaaa"))
  }
}

class KonfigSpec extends FlatSpec with Matchers {
  "Map reader" should "work" in {
    ConfigFactory.parseString(
      """
        |m {
        |  aaa = bbb
        |  ccc.ddd = eee.fff
        |  a.b.c = 1
        |  a.e.f.g = 2
        |  a.j.k.l = ppp
        |}
      """.stripMargin
    )
      .read[Map[String, String]]("m") should be(Map(
        "aaa" -> "bbb",
        "ccc.ddd" -> "eee.fff",
        "a.b.c" -> "1",
        "a.e.f.g" -> "2",
        "a.j.k.l" -> "ppp"
      ))

    ConfigFactory.parseString("b = [ 1, 2, 3 ]")
      .read[List[Int]]("b") should be(List(1, 2, 3))

    ConfigFactory.parseString("b = [ 1, 2, 3 ]")
      .read[Set[Int]]("b") should be(Set(1, 2, 3))

    ConfigFactory.parseString("b = [ 1, 2, 3 ]")
      .read[Vector[Int]]("b") should be(Vector(1, 2, 3))

    ConfigFactory.parseString("n = 3.14159265358979323846264338327950288")
      .read[BigDecimal]("n") should be(BigDecimal("3.14159265358979323846264338327950288"))

    ConfigFactory.parseString("f = 2.71828182846")
      .read[Double]("f") should be(2.71828182846)

    ConfigFactory.parseString("d = 5 day")
      .read[FiniteDuration]("d") should be(5.days)

    ConfigFactory.parseString("a = 3s")
      .read[Map[String, FiniteDuration]]() should be(Map("a" -> 3.seconds))
  }
}

