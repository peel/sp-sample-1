package users.main

import cats.data._
import users.config._

object Application {
  val reader: Reader[Http, Application] =
    Reader(Application.apply)

  val fromApplicationConfig: Reader[ApplicationConfig, Application] =
    Http.fromApplicationConfig andThen reader
}

case class Application(
    http: Http
)
