package starterkit

import pureconfig.ConfigSource
import pureconfig.error.ConfigReaderFailures
import pureconfig.generic.auto._

case class ApplicationConfig(env: String, starterConfig: StarterConfig)
case class StarterConfig(starterName: String, starterRepeat: Int)

class ConfigurationException(msg: String) extends Exception(msg)

object ConfigurationException {
  def fromFailures(failures: ConfigReaderFailures): ConfigurationException =
    new ConfigurationException(failures.toList.map(_.description).mkString("[", ",", "]"))
}

object ApplicationConfig {
  lazy val config: ApplicationConfig = {
    ConfigSource.default.load[ApplicationConfig].fold(
      failures => throw ConfigurationException.fromFailures(failures),
      identity
    )
  }

  lazy val ENV: String = config.env
  lazy val STARTER_CONFIG: StarterConfig = config.starterConfig
}
