package timezra.dropbox.client

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.Matchers
import org.scalatest.BeforeAndAfterAll
import java.io.File
import java.util.Locale
import timezra.dropbox.core.Dropbox
import org.scalatest.FlatSpecLike
import timezra.dropbox.core.Implicits
import akka.actor.ActorRef
import scalaz.effect.IoExceptionOr
import scalaz.iteratee.EnumeratorT
import scalaz.effect.IO
import scala.concurrent.Future
import timezra.dropbox.core.ContentMetadata
import akka.util.Timeout

@RunWith(classOf[JUnitRunner])
class UploaderSpec extends FlatSpecLike with Matchers with BeforeAndAfterAll {

  it should "upload a file" in {
    var howManyTimes = 0
    
    val stub = new Dropbox(null, null) {
      override def putFile(conduit: ActorRef, root: String, path: String, contents: EnumeratorT[IoExceptionOr[(Array[Byte], Int)], IO], length: Int, rev: Option[String], parent_rev: Option[String], overwrite: Option[Boolean])(implicit timeout: Timeout, locale: Option[Locale]): Future[ContentMetadata] = {
        howManyTimes += 1
//        println(s">>>>>>>>>>>>>>>> Called putFile ${path}")
        return null
      }
    }
    val uploader = new Uploader(stub)

    uploader putDirectory (new File("src/test/resources"), "/", 100)
    
    howManyTimes shouldBe (100)
  }
}