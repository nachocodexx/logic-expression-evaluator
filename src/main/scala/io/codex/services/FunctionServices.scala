package io.codex.services

import cats.effect.Sync
import io.chrisdavenport.log4cats.SelfAwareStructuredLogger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import io.chrisdavenport.log4cats.Logger
import cats.effect._
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.io._
import fs2.Stream
import io.codex.encoders.JsonSupport.{FunctionItemResponse, FunctionResponse, RelationsResponse}
import io.codex.sets.Setx
import io.codex.utils.Utils.{booleanToString, booleanTupleToString, extractWordsFromString, listToTuple}
import io.codex.functions.Functions
import io.circe.syntax._
import io.circe.generic.auto._
import io.codex.decoders.Decoders._

import scala.concurrent.duration._
import scala.language.postfixOps

object FunctionServices {
  implicit def usageLogger[F[_]:Sync]: SelfAwareStructuredLogger[F] = Slf4jLogger.unsafeCreate[F]

  def test()(implicit timer: Timer[IO]): Stream[IO, String] =
    Stream.awakeEvery[IO](1 seconds).map(_.toString)

  def apply()(implicit timer:Timer[IO]):HttpRoutes[IO] = HttpRoutes.of {
    case GET -> Root / "stream" => Ok(test())

    case req @ POST -> Root / "option1" => for {
      data <- req.as[SetsData]
      x <- extractWordsFromString(data.A).map(Setx(_))
      y <- extractWordsFromString(data.B).map(Setx(_))
      axb <- IO(x ~* y.members)
      powerSet <- IO(Setx.powerset(axb).members.filterNot(_.cardinality==0))
      relations <- IO(powerSet)
      processedRelations <- Functions.processRelations(relations,x,y)
      json <-IO(FunctionResponse(processedRelations).asJson)
      response <- Ok(json)
    } yield response

    case req @ POST -> Root / "option2"=> for {
      data <- req.as[SetsData]
      x <- extractWordsFromString(data.A).map(Setx(_))
      y <- extractWordsFromString(data.B).map(Setx(_))
      axb <- IO(x ~* y.members)
      powerSet <- IO(Setx.powerset(axb).members.filterNot(_.cardinality==0))
      relations <- IO(powerSet)
      processedRelations <- Functions.processRelations(relations,x,y).map(_.filter(_.isFunction=="Yes"))
      data <- IO(FunctionResponse(data = processedRelations).asJson)
      response <- Ok(data)
    } yield response

    case req @ POST -> Root / "option3" => for {
      data <- req.as[SetsData]
      x <- extractWordsFromString(data.A).map(Setx(_))
      y <- extractWordsFromString(data.B).map(Setx(_))
      axb <- IO(x ~* y.members)
      powerSet <- IO(Setx.powerset(axb).members.filterNot(_.cardinality==0))
      relations <- IO(powerSet)
      processedRelations <- Functions.processRelations(relations,x,y).map(_.filter(_.isInjective=="Yes"))
      data <- IO(FunctionResponse(data = processedRelations).asJson)
      response <- Ok(data)
    } yield response
    case req @ POST -> Root / "option4" => for {
      data <- req.as[SetsData]
      x <- extractWordsFromString(data.A).map(Setx(_))
      y <- extractWordsFromString(data.B).map(Setx(_))
      axb <- IO(x ~* y.members)
      powerSet <- IO(Setx.powerset(axb).members.filterNot(_.cardinality==0))
      relations <- IO(powerSet)
      processedRelations <- Functions.processRelations(relations,x,y).map(_.filter(_.isSurjective=="Yes"))
      data <- IO(FunctionResponse(data = processedRelations).asJson)
      response <- Ok(data)
    } yield response
    case req @ POST -> Root / "option5" => for {
      data <- req.as[SetsData]
      x <- extractWordsFromString(data.A).map(Setx(_))
      y <- extractWordsFromString(data.B).map(Setx(_))
      axb <- IO(x ~* y.members)
      powerSet <- IO(Setx.powerset(axb).members.filterNot(_.cardinality==0))
      relations <- IO(powerSet)
      processedRelations <- Functions.processRelations(relations,x,y).map(_.filter(_.isBijective=="Yes"))
      data <- IO(FunctionResponse(data = processedRelations).asJson)
      response <- Ok(data)
    } yield response

    case req @ POST -> Root / "option6" => for {
      data <- req.as[OrderedPairData]
      x <- extractWordsFromString(data.A)
      xTuple <- IO(listToTuple(x)).map(_.getOrElse(Nil))
      domain <-Functions.getDomain(xTuple)
      codomain <-Functions.getCodomain(xTuple)
      isRelation <- IO((domain & codomain)).map(_.cardinality).map(_==0).map(booleanToString)
      _<-Logger[IO].info(isRelation)
      _<-Logger[IO].info(x.toString)
      _<-Logger[IO].info(xTuple.toString)
      data <- IO(booleanTupleToString(Functions.getCartesianProductSubsetInfo(Setx(xTuple),domain,codomain))).map{
        x=> if(isRelation=="No") ("No","No","No","No") else x
      }
      response <- Ok(FunctionItemResponse(xTuple,isRelation,data._1,data._2,data._3,data._4).asJson)
    } yield response

  }



}
