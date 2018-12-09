package modules.db

import driver.arangodb.ArangoClient
import play.api.{Configuration, Environment}
import play.api.inject.{Binding, Module}

class ArangoModule extends Module {
  override def bindings(environment: Environment, configuration: Configuration): Seq[Binding[_]] = Seq (
    bind[ArangoClient].toSelf
  )
}
