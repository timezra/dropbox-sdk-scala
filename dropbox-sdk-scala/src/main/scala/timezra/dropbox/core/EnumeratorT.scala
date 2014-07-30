package timezra.dropbox.core

import java.io.InputStream
import scalaz.MonadPartialOrder
import scalaz.iteratee.{ EnumeratorT ⇒ E }
import scalaz.effect.IoExceptionOr
import scalaz.effect.IO
import scalaz.iteratee.StepT
import scalaz.iteratee.Iteratee.elInput
import java.io.File

object EnumeratorT extends EnumeratorTFunctions

trait EnumeratorTFunctions {

  def enumInputStream[F[_]](i: ⇒ InputStream, chunkSize: Int = 1048576)(implicit MO: MonadPartialOrder[F, IO]): E[IoExceptionOr[(Array[Byte], Int)], F] =
    new E[IoExceptionOr[(Array[Byte], Int)], F] {
      import MO._
      lazy val inputStream = i
      lazy val buffer = new Array[Byte](chunkSize)
      def apply[A] = (s: StepT[IoExceptionOr[(Array[Byte], Int)], F, A]) ⇒
        s.mapCont(
          k ⇒ {
            val i = IoExceptionOr(buffer, inputStream read buffer)
            if (i exists (_._2 > 0)) k(elInput(i)) >>== apply[A]
            else s.pointI
          }
        )
    }
}

object Implicits {
  implicit def string2Enum[F[_]](s: String)(implicit MO: MonadPartialOrder[F, IO]): E[IoExceptionOr[(Array[Byte], Int)], F] = {
    import EnumeratorT._
    import java.io.ByteArrayInputStream
    enumInputStream[F](new ByteArrayInputStream(s getBytes))
  }
  implicit def file2Enum[F[_]](f: File)(implicit MO: MonadPartialOrder[F, IO]): E[IoExceptionOr[(Array[Byte], Int)], F] = {
    import EnumeratorT._
    import java.io.FileInputStream
    enumInputStream[F](new FileInputStream(f))
  }
}