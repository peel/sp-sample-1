package users

import cats.implicits._
import cats.effect._

import users.config._
import users.main._

object Main extends IOApp {

  val config = ApplicationConfig(
    executors = ExecutorsConfig(
      services = ExecutorsConfig.ServicesConfig(
        parallellism = 4
      )
    ),
    services = ServicesConfig(
      users = ServicesConfig.UsersConfig(
        failureProbability = 0.0,
        timeoutProbability = 0.0
      )
    ),
    http = HttpConfig(
      endpoint = HttpConfig.EndpointConfig(
        host = "localhost",
        port = 9999
      )
    ),
    api = ApiConfig(
      format = ApiConfig.FormatConfig(
        version = "v1"
      )
    )
  )

  def run(args: List[String]): IO[ExitCode] =
    Application.fromApplicationConfig
      .run(config)
      .http
      .stream
      .compile
      .drain
      .as(ExitCode.Success)

}
