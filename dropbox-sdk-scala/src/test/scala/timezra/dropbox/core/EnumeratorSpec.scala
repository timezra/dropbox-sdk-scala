package timezra.dropbox.core

import java.io.ByteArrayInputStream
import org.junit.runner.RunWith
import org.scalatest.Finders
import org.scalatest.FunSpec
import org.scalatest.Matchers
import Arrays.pimpArray
import EnumeratorT.enumInputStream
import akka.util.ByteString
import akka.util.ByteStringBuilder
import scalaz.effect.IO
import scalaz.effect.IoExceptionOr
import scalaz.iteratee.IterateeT.fold
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EnumeratorSpec extends FunSpec with Matchers {

  describe("Enumerate InputStream") {
    it("should enumerate the contents of the input stream") {
      import EnumeratorT._

      val contents = "ce n'est pas un test" getBytes

      val enum = enumInputStream[IO](new ByteArrayInputStream(contents), 6)

      import Arrays._
      val actual = (fold[IoExceptionOr[(Array[Byte], Int)], IO, ByteStringBuilder](ByteString.newBuilder)((b, e) ⇒ e.fold(throw _, a ⇒ b ++= (a._1 takeT a._2))) &= enum)
        .run
        .unsafePerformIO

      actual.result should be(ByteString(contents))
    }
  }
}