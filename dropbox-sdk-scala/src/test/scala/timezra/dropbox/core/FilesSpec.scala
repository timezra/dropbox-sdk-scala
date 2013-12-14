package timezra.dropbox.core

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import org.junit.runner.RunWith
import org.scalatest.BeforeAndAfterAll
import org.scalatest.FunSpecLike
import org.scalatest.junit.JUnitRunner
import org.scalatest.Matchers
import akka.actor.ActorSystem
import akka.io.IO
import akka.testkit.TestKit
import akka.testkit.TestProbe
import spray.can.Http
import spray.http.ContentType.apply
import spray.http.HttpEntity
import spray.http.HttpHeaders.RawHeader
import spray.http.HttpRequest
import spray.http.HttpResponse
import spray.http.parser.HttpParser
import spray.http.StatusCodes
import spray.httpx.UnsuccessfulResponseException

@RunWith(classOf[JUnitRunner])
class FilesSpec extends CoreSpec {

  val Root = "root"
  val Path = "test.txt"
  val Rev = "test"

  describe("Files") {
    it("should make an http request") {
      val probe = TestProbe()
      probe watch IO(Http)

      Dropbox(ClientIdentifier, AccessToken) getFile (probe.ref, Root, Path, Rev)

      val expectedURI = s"https://api-content.dropbox.com/1/files/$Root/$Path?rev=$Rev"
      val expectedHeaders = List(header("Authorization", s"Bearer $AccessToken"), header("User-Agent", s"$ClientIdentifier Dropbox-Scala-SDK/1.0"))
      probe expectMsg HttpRequest(uri = expectedURI, headers = expectedHeaders)
    }
  }
}