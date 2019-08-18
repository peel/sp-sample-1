package users

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

import users.main.Services
import users.services.usermanagement.Error
import users.config._
import users.domain._
import scala.concurrent.Future

package object api {

  def errorCode(err: Error) = err match {
    case Error.Exists             => Conflict("User already exists")
    case Error.NotFound           => NotFound("User not found")
    case Error.Active             => Conflict("User already active")
    case Error.Deleted            => Gone("User already deleted")
    case Error.Blocked            => Conflict("User already blocked")
    case Error.NotModified        => Conflict("Unable to modify user")
    case Error.System(underlying) => InternalServerError(underlying.getMessage())
  }

  def handle[A](f: => Future[Either[Error, A]])(implicit ec: Encoder[A]): IO[Response[IO]] =
    handle(withIO(f))
  def handle[A](io: IO[Either[Error, A]])(implicit ec: Encoder[A]): IO[Response[IO]] = io.flatMap(respond(_))
  def withIO[A](f: => Future[Either[Error, A]]): IO[Either[Error, A]] = IO.fromFuture(IO(f))
  def respond[A, B](res: Either[Error, A])(implicit encoder: Encoder[A]) = res match {
    case Right(a)  => Ok(a.asJson)
    case Left(err) => errorCode(err)
  }

  case class CreateUser(userName: UserName, emailAddress: EmailAddress, password: Password)

  implicit val createUserDecoder = jsonOf[IO, CreateUser]
  implicit val emailAddressDecoder = jsonOf[IO, EmailAddress]
  implicit val passwordDecoder = jsonOf[IO, Password]

  object UserIdVar {
    def unapply(str: String): Option[User.Id] = if (!str.isEmpty) User.Id(str).some else None
  }

  object OptionalPasswordResetParam extends OptionalQueryParamDecoderMatcher[Boolean]("reset")
}
