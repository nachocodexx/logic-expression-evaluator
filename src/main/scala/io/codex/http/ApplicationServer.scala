package io.codex.http

import cats.data.Kleisli
import cats.effect.IO
import org.http4s.implicits._
import org.http4s.dsl.io._
import org.http4s.{HttpRoutes, Request, Response}
import io.codex.evaluator.LogicExpressionEvaluator.{evaluate, isContingencyStr, isContradictionStr, isSatisfiableStr, isTautologyStr}
import io.circe.syntax._
import org.http4s.circe._

//mport io.circe._
//import io.circe.literal._
import io.codex.encoders.JsonSupport._
import io.circe.syntax._
import io.circe._
import io.circe.literal._
import org.http4s.server.middleware.CORS
import io.circe.generic.auto._


object ApplicationServer {


//  def cqrsService = CORS(
//    HttpRoutes.of[IO]{
//      case GET -> Root / "" =>
//        Ok("GER")
//    }
//
//  )

  def apply(): Kleisli[IO, Request[IO], Response[IO]] =
   CORS(HttpRoutes.of[IO] {
      case GET -> Root / "logic" / logicStatement =>
        val (result,data) = evaluate(logicStatement)

        Ok(LogicExpressionResponse(
          logicStatement,
          data,
          isTautologyStr(result),
          isContradictionStr(result),
          isContingencyStr(result),
          isSatisfiableStr(result)
        ).asJson)
//      case POST -> Root /"process" =>
//        Ok("HOLA POST")
    }).orNotFound

}
