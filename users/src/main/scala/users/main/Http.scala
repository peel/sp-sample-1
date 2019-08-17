package users.main

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

import users.config._
import users.domain._

case class CreateUser(userName: UserName, emailAddress: EmailAddress, password: Password)
case class StatusChange(value: User.Status)

object Http {
  val reader: Reader[(HttpConfig, Services), Http] =
    Reader((Http.apply _).tupled)

  val fromApplicationConfig: Reader[ApplicationConfig, Http] =
    (for {
      config <- HttpConfig.fromApplicationConfig
      services <- Services.fromApplicationConfig
    } yield (config, services)) andThen reader

  implicit val createUserDecoder = jsonOf[IO, CreateUser]
  implicit val emailAddressDecoder = jsonOf[IO, EmailAddress]
  implicit val passwordDecoder = jsonOf[IO, Password]
  implicit val statusChangeDecoder = jsonOf[IO, StatusChange]

  object UserIdVar {
    def unapply(str: String): Option[User.Id] = User.Id(str).some
  }
  object OptionalPasswordResetParam extends OptionalQueryParamDecoderMatcher[Boolean]("reset")

  import users.services.usermanagement.Error
  def errorCode(err: Error) = err match {
    case Error.Exists             => Conflict("User already exists")
    case Error.NotFound           => NotFound("User not found")
    case Error.Active             => Conflict("User already active")
    case Error.Deleted            => Gone("User already deleted")
    case Error.Blocked            => Conflict("User already blocked")
    case Error.System(underlying) => InternalServerError(underlying.getMessage())
  }
}

case class Http(
    config: HttpConfig,
    services: Services
) {
  import services._
  import Http._

  implicit val ec = executors.serviceExecutor

  private def publicRoutes =
    HttpRoutes
      .of[IO] {
        case req @ POST -> Root / "users" =>
          (for {
            create <- req.as[CreateUser]
            account <- IO.fromFuture(
              IO(userManagement.signUp(create.userName, create.emailAddress, create.password.some))
            )
          } yield account) flatMap {
            case Right(res) => Ok(res.asJson)
            case Left(err)  => errorCode(err)
          }
      }
      .orNotFound

  private def userRoutes =
    HttpRoutes
      .of[IO] {
        case GET -> Root / "users" / UserIdVar(id) =>
          IO.fromFuture(IO(userManagement.get(id))) flatMap {
            case Right(res) => Ok(res.asJson)
            case Left(err)  => errorCode(err)
          }
        case DELETE -> Root / "users" / UserIdVar(id) =>
          IO.fromFuture(IO(userManagement.delete(id))) flatMap {
            case Right(res) => Gone()
            case Left(err)  => errorCode(err)
          }
        case req @ PUT -> Root / "users" / UserIdVar(id) / "email" =>
          (for {
            email <- req.as[EmailAddress]
            updated <- IO.fromFuture(IO(userManagement.updateEmail(id, email)))
          } yield updated) flatMap {
            case Right(res) => Ok(res.asJson)
            case Left(err)  => errorCode(err)
          }
        case req @ PUT -> Root / "users" / UserIdVar(id) / "password" :? OptionalPasswordResetParam(maybeReset) =>
          (maybeReset match {
            case None =>
              for {
                password <- req.as[Password]
                user <- IO.fromFuture(IO(userManagement.updatePassword(id, password)))
              } yield user
            case Some(_) =>
              IO.fromFuture(IO(userManagement.resetPassword(id)))
          }) flatMap {
            case Right(res) => Ok(res.asJson)
            case Left(err)  => errorCode(err)
          }
      }
      .orNotFound

  private def adminRoutes =
    HttpRoutes
      .of[IO] {
        case GET -> Root / "users" =>
          IO.fromFuture(IO(userManagement.all())).flatMap {
            case Right(res) => Ok(res.asJson)
            case Left(err)  => errorCode(err)
          }
        case req @ PUT -> Root / "admin" / "users" / UserIdVar(id) / "status" =>
          (for {
            update <- req.as[StatusChange]
            user <- IO.fromFuture(IO(userManagement.get(id)))
          } yield user) flatMap {
            case Right(user) if user.isActive =>
              IO.fromFuture(IO(userManagement.block(id)))
            case Right(user) if user.isBlocked =>
              IO.fromFuture(IO(userManagement.unblock(id)))
            case u => IO.pure(u)
          } flatMap {
            case Right(res) => Ok(res.asJson)
            case Left(err)  => errorCode(err)
          }
      }
      .orNotFound

  def stream(implicit t: Timer[IO], m: ConcurrentEffect[IO]) =
    BlazeServerBuilder[IO]
      .bindHttp(config.endpoint.port, config.endpoint.host)
      .withHttpApp(publicRoutes)
      .withHttpApp(userRoutes) // needs AuthMiddleware
      .withHttpApp(adminRoutes) // needs AuthMiddleware + special role
      .serve
}
