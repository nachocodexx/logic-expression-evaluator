package io.codex.services

import cats.implicits._
import cats.effect.implicits._
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
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder

import scala.concurrent.duration._
import scala.language.postfixOps

object FunctionServices {
  implicit def usageLogger[F[_]:Sync]: SelfAwareStructuredLogger[F] = Slf4jLogger.unsafeCreate[F]

  def test()(implicit timer: Timer[IO]) = {
    val data = List.fill(100000)(FunctionItemResponse(Nil,"Yes","Yes","Yes","Yes","Yes"))
    Stream.emits(data).evalMap(x=>IO(x))
  }

  //      Stream.awakeEvery[IO](1 seconds).map(_.toString)

  def apply()(implicit timer:Timer[IO], cs:ContextShift[IO]):HttpRoutes[IO] = HttpRoutes.of {
    case GET -> Root / "stream" => Ok(test())

    case req @ POST -> Root / "option0" =>for {
        data <- req.as[SetsData]
        x <- extractWordsFromString(data.A).map(Setx(_))
        y <- extractWordsFromString(data.B).map(Setx(_))
        axb <- IO(x ~* y.members)
        powerSet <- IO(Setx.powerset(axb).members)
        relations <- IO(powerSet)
        processedRelations <- Functions.processRelations(relations,x,y)
              json <-IO(FunctionResponse(processedRelations).asJson)
//        json <- Stream.emits(processedRelations).evalMap((x)=>IO(""))
        response <- Ok(json)
//        response <- Ok("")
      } yield response

//      val stream = relations.map(x=>Stream.emits(x).evalMap(x=>IO(x)))
//      Ok(stream.unsafeRunSync())

    case req @ POST -> Root / "option1"=> for {
      data <- req.as[SetsData]
      x <- extractWordsFromString(data.A).map(Setx(_))
      y <- extractWordsFromString(data.B).map(Setx(_))
      axb <- IO(x ~* y.members)
      powerSet <- IO(Setx.powerset(axb).members)
      relations <- IO(powerSet)
      processedRelations <- Functions.processRelations(relations,x,y).map(_.filter(_.isFunction=="Yes"))
      data <- IO(FunctionResponse(data = processedRelations).asJson)
      response <- Ok(data)
    } yield response

    case req @ POST -> Root / "option2" => for {
      data <- req.as[SetsData]
      x <- extractWordsFromString(data.A).map(Setx(_))
      y <- extractWordsFromString(data.B).map(Setx(_))
      axb <- IO(x ~* y.members)
      powerSet <- IO(Setx.powerset(axb).members)
      relations <- IO(powerSet)
      processedRelations <- Functions.processRelations(relations,x,y).map(_.filter(_.isInjective=="Yes"))
      data <- IO(FunctionResponse(data = processedRelations).asJson)
      response <- Ok(data)
    } yield response
    case req @ POST -> Root / "option3" => for {
      data <- req.as[SetsData]
      x <- extractWordsFromString(data.A).map(Setx(_))
      y <- extractWordsFromString(data.B).map(Setx(_))
      axb <- IO(x ~* y.members)
      powerSet <- IO(Setx.powerset(axb).members)
      relations <- IO(powerSet)
      processedRelations <- Functions.processRelations(relations,x,y).map(_.filter(_.isSurjective=="Yes"))
      data <- IO(FunctionResponse(data = processedRelations).asJson)
      response <- Ok(data)
    } yield response
    case req @ POST -> Root / "option4" => for {
      data <- req.as[SetsData]
      x <- extractWordsFromString(data.A).map(Setx(_))
      y <- extractWordsFromString(data.B).map(Setx(_))
      axb <- IO(x ~* y.members)
      powerSet <- IO(Setx.powerset(axb).members)
      relations <- IO(powerSet)
      processedRelations <- Functions.processRelations(relations,x,y).map(_.filter(_.isBijective=="Yes"))
      data <- IO(FunctionResponse(data = processedRelations).asJson)
      response <- Ok(data)
    } yield response

//    case req @ POST -> Root / "option5" => for {
//      data <- req.as[OrderedPairData]
//      x <- extractWordsFromString(data.A)
//      xTuple <- IO(listToTuple(x)).map(_.map(_.distinct).getOrElse(Nil))
//      domain <-Functions.getDomain(xTuple)
//      codomain <-Functions.getCodomain(xTuple)
//      isRelation <- IO((domain & codomain)).map(_.cardinality).map(_==0).map(booleanToString)
//      data <- IO(booleanTupleToString(Functions.getCartesianProductSubsetInfo(Setx(xTuple),domain,codomain))).map{
//        x=> if(isRelation=="No") ("No","No","No","No") else x
//      }
//      response <- Ok(FunctionResponse(data=FunctionItemResponse(xTuple,isRelation,data._1,data._2,data._3,data._4)
//        ::Nil)
//        .asJson)
//    } yield response

    case req @ POST -> Root / "option5" => for {
      data <- req.as[FunctionData05]
      x <- extractWordsFromString(data.A).map(Setx(_))
      y <- extractWordsFromString(data.B).map(Setx(_))
      axb <- (x ~* y.members).pure[IO]
      //      _<-Logger[IO].info(axb.toString)
      orderedPairStr <- extractWordsFromString(data.orderedPairData)
      xTuple <- IO(listToTuple(orderedPairStr)).map(_.map(_.distinct).getOrElse(Nil))
      xTupleSetx <- Setx(xTuple).pure[IO]
      notRelations <-  (Setx(xTuple)-Setx(axb)).pure[IO]
      filteredAxB <- (Setx(xTuple)-notRelations).pure[IO]
      domain <-Functions.getDomain(filteredAxB.members)
      codomain <-Functions.getCodomain(filteredAxB.members)
      _<- Logger[IO].info(filteredAxB.toString)
      data <- booleanTupleToString(Functions.getCartesianProductSubsetInfo(xTupleSetx,domain,codomain)).pure[IO]
//      ___________________________________________________________________________________________
      isRelation <- booleanToString(notRelations.cardinality==0).pure[IO]
      response <- Ok(FunctionResponse(data=FunctionItemResponse(xTuple,isRelation,data._1,data._2,data
        ._3,data._4)
        ::Nil)
        .asJson)
    } yield response

  }



}
