package users.api

import cats.data._
import cats.implicits._


import org.http4s.implicits._

import users.config._

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
