import cats.effect._
import io.codex.http.ApplicationServer
import org.http4s.server.blaze.BlazeServerBuilder
import scala.concurrent.ExecutionContext.global

object Application extends IOApp {
  def runServer(): IO[ExitCode] = for {
    server <- BlazeServerBuilder[IO](global)
      .bindHttp(8080,"0.0.0.0")
      .withHttpApp(ApplicationServer())
      .resource
      .use(_=>IO.never)
      .as(ExitCode.Success)
  } yield server

  override def run(args: List[String]): IO[ExitCode] = runServer()
}
