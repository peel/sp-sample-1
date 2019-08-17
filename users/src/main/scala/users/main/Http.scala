package users.main

import scala.concurrent.duration._
import scala.language.postfixOps

import cats.data._
import cats.effect._
import cats.implicits._

import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._

import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze._

import users.services.usermanagement.Error
import users.config._
import users.domain._
import users.api._
import Http._

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
