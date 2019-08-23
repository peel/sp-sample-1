package users.config

import cats.data._

case class ApplicationConfig(
    executors: ExecutorsConfig,
    services: ServicesConfig,
    http: HttpConfig,
    api: ApiConfig
)

case class ExecutorsConfig(
    services: ExecutorsConfig.ServicesConfig
)

object ExecutorsConfig {
  val fromApplicationConfig: Reader[ApplicationConfig, ExecutorsConfig] =
    Reader(_.executors)

  case class ServicesConfig(
      parallellism: Int
  )
}

case class ServicesConfig(
    users: ServicesConfig.UsersConfig
)

object ServicesConfig {
  val fromApplicationConfig: Reader[ApplicationConfig, ServicesConfig] =
    Reader(_.services)

  case class UsersConfig(
      failureProbability: Double,
      timeoutProbability: Double
  )
}

case class HttpConfig(
    endpoint: HttpConfig.EndpointConfig
)

object HttpConfig {
  val fromApplicationConfig: Reader[ApplicationConfig, HttpConfig] =
    Reader(_.http)

  case class EndpointConfig(
      host: String,
      port: Int
  )
}

case class ApiConfig(
    format: ApiConfig.FormatConfig
)

object ApiConfig {
  val fromApplicationConfig: Reader[ApplicationConfig, ApiConfig] =
    Reader(_.api)

  case class FormatConfig(
      version: String
  )
}
