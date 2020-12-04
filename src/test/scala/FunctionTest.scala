import cats.effect.implicits._
import cats.implicits._
import cats.effect.{IO, Sync}
import io.codex.decoders.Decoders.FunctionData05
import org.scalatest.funsuite.AnyFunSuite
import io.codex.functions.Functions.{CartesianProductSubset, getCartesianProductSubsetInfo}
import io.codex.sets.Setx
import io.codex.utils.Utils.{booleanTupleToString, extractWordsFromString, listToTuple}
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import io.codex.encoders.JsonSupport.{FunctionItemResponse, FunctionResponse}
import io.codex.functions.Functions
import org.http4s.dsl.io.Ok
class FunctionTest  extends  AnyFunSuite{
  test("Functions"){
    val relations = Setx(("X","1")::("W","1")::("Y","2")::Nil)
    val domain = Setx("X"::"W"::"Y"::Nil)
    val codomain = Setx("1"::"2"::Nil)
    val response = getCartesianProductSubsetInfo(relations,domain,codomain)
    println(response)

      assert(true)
  }

  test("tuples"){
    val text:String = "(1,x),(1,x),(4,y)"
    val res = extractWordsFromString(text).unsafeRunSync()
    val t = listToTuple(res).map(_.distinct)
    println(t)

//    val t = "1"::"2"::"3"::Nil
//    val nt = listToTuple(t)
//    println(nt)
    assert(true)
  }

  test("option 6"){
    implicit def unsafeLogger[F[_]: Sync] = Slf4jLogger.getLogger[F]
    val result = for {
//      data <- FunctionData05("Mario,Pedro,Laura","Rojo,Verde,Azul","(Mario,Negro),(Pedro,Azul)").pure[IO]
//      data <- FunctionData05("Mario,Pedro,Laura","Rojo,Verde,Azul","(Mario,Negro)").pure[IO]
      data <- FunctionData05("Mario,Pedro,Laura","Rojo,Verde,Azul","(Mario,Azul),(PEDO,Azul)").pure[IO]
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
      _<-Logger[IO].info(data.toString)
    } yield axb

    result.unsafeRunSync()

    assert(true)
  }

}
