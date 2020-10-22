import org.http4s.implicits.http4sLiteralsSyntax
import io.codex.services.SetsServices
import org.scalatest.funsuite.AnyFunSuite
import cats.implicits._
import io.circe._
import io.circe.syntax._
import io.circe.generic.semiauto._
import cats.effect._
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.io._
import org.http4s.implicits._

class HttpTest extends AnyFunSuite{
  val rootRequest: Request[IO] = Request[IO](Method.GET,uri = uri"/")
  val testRequest: Request[IO] = Request[IO](Method.POST,uri = uri"/test2")

  test("SIMPLE ROUTE"){
    val setsServices=SetsServices()

  }

}
