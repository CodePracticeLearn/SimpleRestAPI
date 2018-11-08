import cats.effect.IO
import cats.Monad
import cats.FlatMap
import cats.implicits._
import cats.effect._
import fs2.StreamApp
import fs2.Stream
import io.circe.generic.auto._
import io.circe.syntax._

import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.dsl.io._

object Server extends StreamApp[IO] with Http4sDsl[IO] {

  val Message = "Message"

  def service[F[_]](empUtil: Utility[F])(implicit F: Effect[F]) = HttpService[F] {

    case GET -> Root / Message / empId =>
      empUtil.getId(empId)
          .flatMap{
      case Some(value) => Response(status = Status.Ok).withBody(value.asJson)
      case None      => F.pure(Response(status = Status.NotFound))
    }

    case req @ POST -> Root / Message =>
         req.decodeJson[EmpName]
           .flatMap(empUtil.addName)
           .flatMap(value => Response(status = Status.Created).withBody(value.asJson))

    case req @ PUT -> Root / Message =>
      req.decodeJson[EmplIdName]
        .flatMap(empUtil.updateIdName)
        .flatMap(_ => F.pure(Response(status = Status.Ok)))

    case DELETE -> Root / Message / empId =>
      empUtil.delete(empId)
        .flatMap(_ => F.pure(Response(status = Status.NoContent)))
  }

  def stream(args: List[String], requestShutdown: IO[Unit]) =
    Stream.eval(Utility.empty[IO]).flatMap { util =>
    BlazeBuilder[IO]
      .bindHttp(8080, "0.0.0.0")
      .mountService(service(util), "/")
      .serve
    }
}
