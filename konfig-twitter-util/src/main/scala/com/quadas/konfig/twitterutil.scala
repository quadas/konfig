package com.quadas.konfig

import com.twitter.util.{ Duration, StorageUnit }
import com.typesafe.config.Config

trait TwitterUtilReaders {
  implicit val durationReader = new ConfigReader[Duration] {
    override def read(c: Config, path: String): Duration = Duration.fromNanoseconds(c.getDuration(path).toNanos)
  }

  implicit val storageUnitReader = new ConfigReader[StorageUnit] {
    override def read(c: Config, path: String): StorageUnit = StorageUnit.fromBytes(c.getBytes(path))
  }
}

package object twitterutil extends TwitterUtilReaders
