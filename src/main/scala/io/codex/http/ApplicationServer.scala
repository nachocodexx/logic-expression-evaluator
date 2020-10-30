package io.codex.http
import cats.implicits._
import org.http4s.server.Router
import cats.data.Kleisli
import cats.effect.{ContextShift, IO, Timer}
import org.http4s.implicits._
import org.http4s.{HttpApp, HttpRoutes, Request, Response, Status}
import org.http4s.server.middleware.CORS
import io.codex.services.{FunctionServices, LogicServices, SetsServices}



object ApplicationServer {

  def getRoutes()(implicit timer:Timer[IO],cs:ContextShift[IO]): HttpRoutes[IO] = Router(
    "api/logic"-> LogicServices(),
    "api/sets" -> SetsServices(),
    "api/functions" -> FunctionServices()
  )

  def apply()(implicit timer:Timer[IO],cs:ContextShift[IO]):Kleisli[IO,Request[IO],Response[IO]]= CORS(getRoutes
    .orNotFound)

}
