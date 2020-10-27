package io.codex.services

import cats.effect._
import io.codex.encoders.JsonSupport.{Cardinalities, CartersianProduct, Difference, Intersection, PowerSets, ProperSubsets, Subsets, SymmetricDifference}
import io.codex.utils.Utils.booleanToString
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.circe._
//
import io.circe.syntax._
import io.circe.generic.auto._
//
import io.chrisdavenport.log4cats.{Logger, SelfAwareStructuredLogger}
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
//
import io.codex.decoders.Decoders._
import io.codex.encoders.JsonSupport.{Members, SetsOperationsResponse}
import io.codex.sets.Setx
import io.codex.utils.Utils.extractWordsFromString
import io.codex.encoders.JsonSupport.SetsOperationsResponse

object SetsServices {
  implicit def usageLogger[F[_]:Sync]: SelfAwareStructuredLogger[F] = Slf4jLogger.unsafeCreate[F]

  def apply():HttpRoutes[IO] =  HttpRoutes.of {
    case req @ POST -> Root  => for {
      data <- req.as[SetsData]
      x <- extractWordsFromString(data.A).map(Setx(_))
      y <- extractWordsFromString(data.B).map(Setx(_))
      cartersianProductAB<-IO(x~*y.members).map(x=>x.map{
        case (x, y) => s"($x,$y)"
      })
      cartersianProductBA<-IO(y~*x.members).map(x=>x.map{
        case (x, y) => s"($x,$y)"
      })
      powerSetA <- IO(Setx.powerset(x.members).members.filter(_.cardinality>0).map(_.members).map(_.mkString(",")).map
      (x=>s"{$x}") )
      powerSetB <- IO(Setx.powerset(x.members).members.filter(_.cardinality>0).map(_.members).map(_.mkString(",")).map
      (x=>s"{$x}") )

      responseJson <- IO(SetsOperationsResponse(
        members = Members(x.members,y.members),
        cardinalities = Cardinalities(x.cardinality,y.cardinality),
        powerSets = PowerSets(powerSetA ,powerSetB),
        comparison = booleanToString(x==y),
        subsets = Subsets( booleanToString(x.subsetOf(y)),booleanToString(y.subsetOf(y))),
        difference = Difference((x-y).members,(y-x).members),
        properSubsets = ProperSubsets( booleanToString(x.isProperSubsetOf(y)),booleanToString(y.isProperSubsetOf(x)) ),
        symmetricDifference = SymmetricDifference((x-&y).members,(y-&x).members),
        union = (x|y).members,
        intersection = Intersection((x&y).members,booleanToString(Setx.areDisjoint(x,y))),
        cartersianProduct = CartersianProduct( cartersianProductAB,cartersianProductBA)

      ))
      response <-Ok(responseJson.asJson)
    } yield response
  }
}
