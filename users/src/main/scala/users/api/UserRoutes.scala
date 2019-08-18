package users.api

import cats.data._
import cats.effect._


import org.http4s._
import org.http4s.dsl.io._

import users.main.Services
import users.config._
import users.domain._

object UserService {
  val reader: Reader[(ApiConfig, Services), UserService] =
    Reader((UserService.apply _).tupled)

  val fromApplicationConfig: Reader[ApplicationConfig, UserService] =
    (for {
      config   <- ApiConfig.fromApplicationConfig
      services <- Services.fromApplicationConfig
    } yield (config, services)) andThen reader
}

case class UserService(config: ApiConfig, services: Services) {
  import services._
  import PublicService._

  lazy val routes =
    HttpRoutes
      .of[IO] {
        case GET -> Root / config.format.version / "users" / UserIdVar(id) =>
          handle {
            userManagement.get(id)
          }
        case DELETE -> Root / config.format.version / "users" / UserIdVar(id) =>
          withIO {
            userManagement.delete(id)
          } flatMap {
            case Right(_)  => Gone()
            case Left(err) => errorCode(err)
          }
        case req @ PUT -> Root / config.format.version / "users" / UserIdVar(id) / "email" =>
          handle {
            for {
              email   <- req.as[EmailAddress]
              updated <- withIO(userManagement.updateEmail(id, email))
            } yield updated
          }
        case req @ PUT -> Root / config.format.version / "users" / UserIdVar(id) / "password" :? OptionalPasswordResetParam(
              maybeReset
            ) =>
          handle {
            maybeReset match {
              case None =>
                for {
                  password <- req.as[Password]
                  user     <- withIO(userManagement.updatePassword(id, password))
                } yield user
              case Some(_) =>
                withIO(userManagement.resetPassword(id))
            }
          }
      }
}
