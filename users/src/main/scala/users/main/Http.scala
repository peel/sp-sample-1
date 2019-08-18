package users.main

import scala.concurrent.duration._
import scala.language.postfixOps

import cats.data._
import cats.effect._


import org.http4s.server.blaze._

import users.config._
import users.api._

object Http {
  val reader: Reader[(HttpConfig, Services, Api), Http] =
    Reader((Http.apply _).tupled)

  val fromApplicationConfig: Reader[ApplicationConfig, Http] =
    (for {
      config   <- HttpConfig.fromApplicationConfig
      services <- Services.fromApplicationConfig
      api      <- Api.fromApplicationConfig
    } yield (config, services, api)) andThen reader
}

case class Http(
    config: HttpConfig,
    services: Services,
    api: Api
) {
  import services._

  implicit val ec = executors.serviceExecutor

  def stream(implicit t: Timer[IO], m: ConcurrentEffect[IO]) =
    BlazeServerBuilder[IO]
      .bindHttp(config.endpoint.port, config.endpoint.host)
      .withIdleTimeout(3 seconds)
      .withHttpApp(api.routes)
      .serve
}
