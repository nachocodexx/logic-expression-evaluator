package io.codex.decoders
import cats.effect._
import org.http4s.circe._
import io.circe.generic.auto._
import org.http4s.EntityDecoder

object Decoders {

  case class SetsData(A:String,B:String)
  implicit val setsDataDecoder: EntityDecoder[IO, SetsData] = jsonOf[IO,SetsData]
  case class OrderedPairData(A:String)
  implicit val orderedPairDataDecoder: EntityDecoder[IO, OrderedPairData] = jsonOf[IO,OrderedPairData]
  case class FunctionData05(A:String,B:String,orderedPairData: String)
  implicit val functionData05Decoder:EntityDecoder[IO,FunctionData05]= jsonOf[IO,FunctionData05]


}
