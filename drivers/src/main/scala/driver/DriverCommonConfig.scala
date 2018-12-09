package driver

import play.api.{ConfigLoader, Configuration}

import scala.collection.JavaConverters._

class DriverCommonConfig[T] (builder: T, configuration: Configuration) {
  def readHosts(path: String, f: PartialFunction[(T, List[(String, Int)]), Unit]): DriverCommonConfig[T] = {
    f (
      builder,
      configuration.underlying.getConfigList(path).asScala.map(obj => (
        obj.getString("host"),
        obj.getInt("port")
      )).toList
    )
    this
  }

  def readValue[V](path: String, f: PartialFunction[(T, Option[V]), Unit])(implicit cfg:ConfigLoader[V]): DriverCommonConfig[T] = {
    configuration.getOptional(path) match {
      case x @ Some(_) => f ((builder, x))
      case _ =>
    }
    this
  }

  def get: T = builder
}

