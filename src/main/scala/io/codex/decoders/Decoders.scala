package io.codex.decoders
import cats.effect._
import org.http4s.circe._
import io.circe.generic.auto._
import org.http4s.EntityDecoder

object Decoders {

  case class SetsData(A:String,B:String)
  implicit val setsDataDecoder: EntityDecoder[IO, SetsData] = jsonOf[IO,SetsData]


}
