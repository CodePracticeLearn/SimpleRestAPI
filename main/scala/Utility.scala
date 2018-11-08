
import java.util.UUID
import cats.effect._
import scala.collection.mutable.ListBuffer
import cats.FlatMap
import cats.implicits._
import cats.effect.IO

final case class Utility[F[_]](private val empIdNames: ListBuffer[EmplIdName])(implicit e: Effect[F]) {
  val makeId: F[String] = e.delay { UUID.randomUUID().toString }

    def getId(id: String): F[Option[EmplIdName]] =
      e.delay { empIdNames.find(_.id == id) }

    def addName(name: EmpName): F[String] =
      for {
        uuid <- makeId
        _ <- e.delay { empIdNames += empIdNam(name, uuid) }
      } yield uuid

    def updateIdName(empIdName: EmplIdName): F[Unit] = {
      for {
        _ <- e.delay { empIdNames -= empIdName }
        _ <- e.delay { empIdNames += empIdName }
      } yield()
    }

    def delete(empId: String): F[Unit] =
      e.delay { empIdNames.find(_.id == empId).foreach(h => empIdNames -= h) }


    def empIdNam(empName: EmpName, id: String): EmplIdName =
      EmplIdName(id, empName.name)
}
