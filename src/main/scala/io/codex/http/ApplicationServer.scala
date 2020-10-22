package io.codex.http
import cats.implicits._
import org.http4s.server.Router
import cats.data.Kleisli
import cats.effect.IO
import org.http4s.implicits._
import org.http4s.dsl.io._
import org.http4s.{HttpApp, HttpRoutes, Request, Response, Status}
import org.http4s.server.middleware.CORS
import io.codex.services.{LogicServices,SetsServices}



object ApplicationServer {

  def getRoutes: HttpRoutes[IO] = Router(
    "logic"-> LogicServices(),
    "sets" -> SetsServices()
  )

  def apply():Kleisli[IO,Request[IO],Response[IO]]= CORS(getRoutes.orNotFound)

}
