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

import users.services.usermanagement.Error
import users.config._
import users.domain._

object Http {
  val reader: Reader[(HttpConfig, Services), Http] =
    Reader((Http.apply _).tupled)

  val fromApplicationConfig: Reader[ApplicationConfig, Http] =
    (for {
      config   <- HttpConfig.fromApplicationConfig
      services <- Services.fromApplicationConfig
    } yield (config, services)) andThen reader

  case class CreateUser(userName: UserName, emailAddress: EmailAddress, password: Password)
  case class StatusChange(value: User.Status)

  implicit val createUserDecoder = jsonOf[IO, CreateUser]
  implicit val emailAddressDecoder = jsonOf[IO, EmailAddress]
  implicit val passwordDecoder = jsonOf[IO, Password]
  implicit val statusChangeDecoder = jsonOf[IO, StatusChange]

  object UserIdVar {
    def unapply(str: String): Option[User.Id] = User.Id(str).some
  }
  object OptionalPasswordResetParam extends OptionalQueryParamDecoderMatcher[Boolean]("reset")

  def errorCode(err: Error) = err match {
    case Error.Exists             => Conflict("User already exists")
    case Error.NotFound           => NotFound("User not found")
    case Error.Active             => Conflict("User already active")
    case Error.Deleted            => Gone("User already deleted")
    case Error.Blocked            => Conflict("User already blocked")
    case Error.System(underlying) => InternalServerError(underlying.getMessage())
  }

  def respond[A, B](res: Either[Error, A])(implicit encoder: Encoder[A]) = res match {
    case Right(a)  => Ok(a.asJson)
    case Left(err) => errorCode(err)
  }
}

case class Http(
    config: HttpConfig,
    services: Services
) {
  import services._
  import Http._

  implicit val ec = executors.serviceExecutor
  val ApiVersion = config.api.version

  def stream(implicit t: Timer[IO], m: ConcurrentEffect[IO]) =
    BlazeServerBuilder[IO]
      .bindHttp(config.endpoint.port, config.endpoint.host)
      .withHttpApp(routes)
      .serve

  private def routes = (publicRoutes <+> userRoutes <+> adminRoutes).orNotFound

  private def publicRoutes =
    HttpRoutes
      .of[IO] {
        case req @ POST -> Root / ApiVersion / "users" =>
          handle {
            for {
              create  <- req.as[CreateUser]
              account <- withIO(userManagement.signUp(create.userName, create.emailAddress, create.password.some))
            } yield account
          }
      }

  private def userRoutes =
    HttpRoutes
      .of[IO] {
        case GET -> Root / ApiVersion / "users" / UserIdVar(id) =>
          handle {
            userManagement.get(id)
          }
        case DELETE -> Root / ApiVersion / "users" / UserIdVar(id) =>
          handle {
            userManagement.delete(id).map(_.flatMap(_ => Left(Error.Deleted)))
          }
        case req @ PUT -> Root / ApiVersion / "users" / UserIdVar(id) / "email" =>
          handle {
            for {
              email   <- req.as[EmailAddress]
              updated <- withIO(userManagement.updateEmail(id, email))
            } yield updated
          }
        case req @ PUT -> Root / ApiVersion / "users" / UserIdVar(id) / "password" :? OptionalPasswordResetParam(
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

  private def adminRoutes =
    HttpRoutes
      .of[IO] {
        case GET -> Root / ApiVersion / "users" =>
          handle {
            userManagement.all()
          }
        case req @ PUT -> Root / ApiVersion / "admin" / "users" / UserIdVar(id) / "status" =>
          handle {
            (for {
              update <- req.as[StatusChange]
              user   <- withIO(userManagement.get(id))
            } yield user) flatMap {
              case Right(user) if user.isActive =>
                withIO(userManagement.block(id))
              case Right(user) if user.isBlocked =>
                withIO(userManagement.unblock(id))
              case u => IO.pure(u)
            }
          }
      }

  private def withIO[A](f: => scala.concurrent.Future[Either[Error, A]]): IO[Either[Error, A]] = IO.fromFuture(IO(f))
  private def handle[A](io: IO[Either[Error, A]])(implicit ec: Encoder[A]): IO[Response[IO]] = io.flatMap(respond(_))
  private def handle[A](f: => scala.concurrent.Future[Either[Error, A]])(implicit ec: Encoder[A]): IO[Response[IO]] =
    handle(withIO(f))

}
