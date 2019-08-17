package users.api

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
import users.api._

object PublicService {
  val reader: Reader[(ApiConfig, Services), PublicService] =
    Reader((PublicService.apply _).tupled)

  val fromApplicationConfig: Reader[ApplicationConfig, PublicService] =
    (for {
      config   <- ApiConfig.fromApplicationConfig
      services <- Services.fromApplicationConfig
    } yield (config, services)) andThen reader

  import io.circe.generic.semiauto._
  implicit val publicUserEncoder: Encoder[User] =
    deriveEncoder[User].mapJsonObject(_.remove("metadata").remove("password"))

}
case class PublicService(config: ApiConfig, services: Services) {
  import services._
  import PublicService._

  lazy val routes =
    HttpRoutes
      .of[IO] {
        case req @ POST -> Root / config.format.version / "users" =>
          (for {
            create  <- req.as[CreateUser]
            account <- withIO(userManagement.signUp(create.userName, create.emailAddress, create.password.some))
          } yield account) flatMap {
            case Right(user) => Created(user.asJson)
            case Left(err)   => errorCode(err)
          }
      }
}
