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
import Http._

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

  object UserIdVar {
    def unapply(str: String): Option[User.Id] = User.Id(str).some
  }
  object OptionalPasswordResetParam extends OptionalQueryParamDecoderMatcher[Boolean]("reset")

}

case class Http(
    config: HttpConfig,
    services: Services
) extends PublicRoutes
    with UserRoutes
    with AdminRoutes {
  import services._

  implicit val ec = executors.serviceExecutor

  def stream(implicit t: Timer[IO], m: ConcurrentEffect[IO]) =
    BlazeServerBuilder[IO]
      .bindHttp(config.endpoint.port, config.endpoint.host)
      .withHttpApp(routes)
      .serve

  private val routes = (publicRoutes <+> userRoutes <+> adminRoutes).orNotFound
}

sealed trait Routes {
  val config: HttpConfig
  val services: Services
  val ApiVersion = config.api.version

  def errorCode(err: Error) = err match {
    case Error.Exists             => Conflict("User already exists")
    case Error.NotFound           => NotFound("User not found")
    case Error.Active             => Conflict("User already active")
    case Error.Deleted            => Gone("User already deleted")
    case Error.Blocked            => Conflict("User already blocked")
    case Error.NotModified        => Conflict("Unable to modify user")
    case Error.System(underlying) => InternalServerError(underlying.getMessage())
  }

  def handle[A](f: => scala.concurrent.Future[Either[Error, A]])(implicit ec: Encoder[A]): IO[Response[IO]] =
    handle(withIO(f))
  def handle[A](io: IO[Either[Error, A]])(implicit ec: Encoder[A]): IO[Response[IO]] = io.flatMap(respond(_))
  def withIO[A](f: => scala.concurrent.Future[Either[Error, A]]): IO[Either[Error, A]] = IO.fromFuture(IO(f))
  def respond[A, B](res: Either[Error, A])(implicit encoder: Encoder[A]) = res match {
    case Right(a)  => Ok(a.asJson)
    case Left(err) => errorCode(err)
  }
}

object PublicRoutes {
  import io.circe.generic.semiauto._
  implicit val userEncoder: Encoder[User] = deriveEncoder[User].mapJsonObject(_.remove("metadata").remove("password"))
}
trait PublicRoutes extends Routes {
  import services._

  val publicRoutes =
    HttpRoutes
      .of[IO] {
        case req @ POST -> Root / ApiVersion / "users" =>
          (for {
            create  <- req.as[CreateUser]
            account <- withIO(userManagement.signUp(create.userName, create.emailAddress, create.password.some))
          } yield account) flatMap {
            case Right(user) => Created(user.asJson)
            case Left(err)   => errorCode(err)
          }
      }
}

trait UserRoutes extends Routes {
  import services._
  import PublicRoutes._

  val userRoutes =
    HttpRoutes
      .of[IO] {
        case GET -> Root / ApiVersion / "users" / UserIdVar(id) =>
          handle {
            userManagement.get(id)
          }
        case DELETE -> Root / ApiVersion / "users" / UserIdVar(id) =>
          withIO {
            userManagement.delete(id)
          } flatMap {
            case Right(_)  => Gone()
            case Left(err) => errorCode(err)
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
}

object AdminRoutes {
  implicit val statusChangeDecoder = jsonOf[IO, StatusChange]
}
trait AdminRoutes extends Routes {
  import services._
  import AdminRoutes._

  val adminRoutes =
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
            } yield (update, user)) flatMap {
              case (StatusChange(User.Status.Active), Right(user)) if user.isBlocked =>
                withIO(userManagement.block(id))
              case (StatusChange(User.Status.Blocked), Right(user)) if user.isActive =>
                withIO(userManagement.unblock(id))
              case (_, _) => IO.pure(Left(Error.NotModified))
            }
          }
      }
}
