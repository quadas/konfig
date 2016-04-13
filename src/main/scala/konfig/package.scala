package konfig

import com.typesafe.config.Config
import shapeless._
import shapeless.labelled.{ FieldType, field }

import scala.concurrent.duration._
import scala.language.implicitConversions

// for primitive-like value
trait ConfigReader[T] {
  def read(c: Config, path: String): T
}

trait SubtypeHint {
  def fieldName(): String
  def matchType(fieldValue: String, typeName: String): Boolean
}

trait KeyStyle {
  def style(key: String): String
}

object KeyStyle {
  object Same extends KeyStyle {
    override def style(key: String): String = key
  }

  // someVeryLongName -> some-very-long-name, ASCII SUPPORT ONLY
  object Dashed extends KeyStyle {
    override def style(key: String): String = {
      val sb = new java.lang.StringBuilder()
      var i = 0
      while (i < key.length) {
        val ch = key.charAt(i)
        if (ch.isLower) {
          sb.append(ch)
        } else {
          sb.append('-').append(ch.toLower)
        }
        i += 1
      }
      sb.toString
    }
  }
}

class EnrichedConfig(val origin: Config) extends AnyVal {
  def read[T](path: String)(implicit cr: ConfigReader[T]): T = {
    cr.read(origin, path)
  }
}

// implicits in StandardReaders have higher priority
object Konfig extends ProductReaders with StandardReaders {
  implicit val keyStyle = KeyStyle.Dashed

  implicit object subtypeHint extends SubtypeHint {
    val LOCALE = java.util.Locale.US
    override def fieldName(): String = "type"
    override def matchType(fieldValue: String, typeName: String): Boolean = typeName.toLowerCase(LOCALE).startsWith(fieldValue.toLowerCase(LOCALE))
  }

  implicit def enrichedConfig(origin: Config): EnrichedConfig = new EnrichedConfig(origin)
}

trait ProductReaders {
  implicit val hNilReader = new ConfigReader[HNil] {
    override def read(c: Config, path: String) = HNil
  }

  implicit def hListReader[Key <: Symbol, Head, Tail <: HList](
    implicit
    key: Witness.Aux[Key],
    keyStyle: KeyStyle,
    cr: Lazy[ConfigReader[Head]],
    tail: Lazy[ConfigReader[Tail]]
  ): ConfigReader[FieldType[Key, Head] :: Tail] = new ConfigReader[FieldType[Key, Head] :: Tail] {
    override def read(c: Config, path: String) = {
      val v = cr.value.read(c.getConfig(path), keyStyle.style(key.value.name))
      field[Key](v) :: tail.value.read(c, path)
    }
  }

  implicit val cNilReader = new ConfigReader[CNil] {
    import com.typesafe.config.ConfigException
    override def read(c: Config, path: String): CNil = throw new ConfigException.Generic("Can't resolve config, this should be a compiler error")
  }

  implicit def coproductReader[Key <: Symbol, Head, Tail <: Coproduct](
    implicit
    key: Witness.Aux[Key],
    subtypeHint: SubtypeHint,
    cr: Lazy[ConfigReader[Head]],
    tail: Lazy[ConfigReader[Tail]]
  ): ConfigReader[FieldType[Key, Head] :+: Tail] = new ConfigReader[FieldType[Key, Head] :+: Tail] {
    override def read(c: Config, path: String) = {
      val subTypeValue = c.getConfig(path).getString(subtypeHint.fieldName())
      if (subtypeHint.matchType(subTypeValue, key.value.name)) {
        Inl(field[Key](cr.value.read(c, path)))
      } else {
        Inr(tail.value.read(c, path))
      }
    }
  }

  implicit def productReader[T, Repr](
    implicit
    gen: LabelledGeneric.Aux[T, Repr],
    cr: Lazy[ConfigReader[Repr]],
    t: Typeable[T]
  ): ConfigReader[T] = new ConfigReader[T] {
    override def read(c: Config, path: String) = {
      gen.from(cr.value.read(c, path))
    }
  }
}

trait StandardReaders {
  implicit object stringReader extends ConfigReader[String] {
    override def read(c: Config, path: String): String = c.getString(path)
  }

  implicit object intReader extends ConfigReader[Int] {
    override def read(c: Config, path: String): Int = c.getInt(path)
  }

  implicit object longReader extends ConfigReader[Long] {
    override def read(c: Config, path: String): Long = c.getLong(path)
  }

  implicit def listReader[T](implicit cr: ConfigReader[T]): ConfigReader[List[T]] = {
    val _PATH = "_"
    new ConfigReader[List[T]] {
      override def read(c: Config, path: String): List[T] = {
        import scala.collection.JavaConverters._
        c.getList(path)
          .asScala
          .map(v => cr.read(v.atKey(_PATH), _PATH))
          .toList
      }
    }
  }

  implicit def setReader[T](implicit cr: ConfigReader[T]): ConfigReader[Set[T]] = {
    new ConfigReader[Set[T]] {
      override def read(c: Config, path: String): Set[T] = listReader(cr).read(c, path).toSet
    }
  }

  implicit def optionReader[T](implicit cr: ConfigReader[T]): ConfigReader[Option[T]] = {
    new ConfigReader[Option[T]] {
      override def read(c: Config, path: String): Option[T] = {
        if (c.hasPath(path)) {
          Some(cr.read(c, path))
        } else None
      }
    }
  }
}
