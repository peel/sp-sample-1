package users.api

import cats.data._
import cats.effect._
import cats.implicits._

import io.circe._
import io.circe.generic.auto._
import io.circe.generic.extras.semiauto.deriveEnumerationDecoder
import io.circe.syntax._

import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.io._

import users.main.Services
import users.services.usermanagement.Error
import users.config._
import users.domain._

sealed trait StatusChange
case object Block extends StatusChange
case object Unblock extends StatusChange

case class StatusChangeRequest(value: StatusChange)

object AdminService {
  val reader: Reader[(ApiConfig, Services), AdminService] =
    Reader((AdminService.apply _).tupled)

  val fromApplicationConfig: Reader[ApplicationConfig, AdminService] =
    (for {
      config   <- ApiConfig.fromApplicationConfig
      services <- Services.fromApplicationConfig
    } yield (config, services)) andThen reader

  implicit val statusChangeDecoder = deriveEnumerationDecoder[StatusChange]
  implicit val statusChangeRequestDecoder = jsonOf[IO, StatusChangeRequest]
}

case class AdminService(config: ApiConfig, services: Services) {
  import services._
  import AdminService._

  lazy val routes =
    HttpRoutes
      .of[IO] {
        case GET -> Root / config.format.version / "admin" / "users" =>
          handle {
            userManagement.all()
          }
        case req @ POST -> Root / config.format.version / "admin" / "users" =>
          (for {
            create  <- req.as[CreateUser]
            account <- withIO(userManagement.signUp(create.userName, create.emailAddress, create.password.some))
          } yield account) flatMap {
            case Right(user) => Created(user.asJson)
            case Left(err)   => errorCode(err)
          }
        case GET -> Root / config.format.version / "admin" / "users" / UserIdVar(id) =>
          handle {
            userManagement.get(id)
          }
        case DELETE -> Root / config.format.version / "admin" / "users" / UserIdVar(id) =>
          withIO {
            userManagement.delete(id)
          } flatMap {
            case Right(_)  => Gone()
            case Left(err) => errorCode(err)
          }
        case req @ PUT -> Root / config.format.version / "admin" / "users" / UserIdVar(id) / "email" =>
          handle {
            for {
              email   <- req.as[EmailAddress]
              updated <- withIO(userManagement.updateEmail(id, email))
            } yield updated
          }
        case req @ PUT -> Root / config.format.version / "admin" / "users" / UserIdVar(id) / "password" :? OptionalPasswordResetParam(
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
        case req @ PUT -> Root / config.format.version / "admin" / "users" / UserIdVar(id) / "status" =>
          handle {
            (for {
              update <- req.as[StatusChangeRequest]
              user   <- withIO(userManagement.get(id))
            } yield (update, user)) flatMap {
              case (StatusChangeRequest(Block), Right(user)) if user.isActive =>
                withIO(userManagement.block(id))
              case (StatusChangeRequest(Unblock), Right(user)) if user.isBlocked =>
                withIO(userManagement.unblock(id))
              case (_, _) => IO.pure(Left(Error.NotModified))
            }
          }
      }
}
