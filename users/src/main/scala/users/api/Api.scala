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

object Api {
  val reader: Reader[(PublicService, UserService, AdminService), Api] =
    Reader((Api.apply _).tupled)

  val fromApplicationConfig: Reader[ApplicationConfig, Api] =
    (for {
      public <- PublicService.fromApplicationConfig
      user   <- UserService.fromApplicationConfig
      admin  <- AdminService.fromApplicationConfig
    } yield (public, user, admin)) andThen reader

}
case class Api(public: PublicService, user: UserService, admin: AdminService) {
  val routes = (
    public.routes <+>
      user.routes <+>
      admin.routes
  ).orNotFound
}
