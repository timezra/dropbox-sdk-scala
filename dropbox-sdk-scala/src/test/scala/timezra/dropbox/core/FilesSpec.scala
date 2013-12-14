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
import spray.http.ContentTypes.`text/plain`
import java.util.Date
import java.text.SimpleDateFormat
import java.text.DateFormat

@RunWith(classOf[JUnitRunner])
class FilesSpec extends CoreSpec {

  val Root = "root"
  val Path = "test.txt"
  val Rev = "test"

  describe("Files") {
    it("should make an http request") {
      val probe = ioProbe

      dropbox getFile (probe.ref, Root, Path, Rev)

      val expectedURI = s"https://api-content.dropbox.com/1/files/$Root/$Path?rev=$Rev"
      val expectedHeaders = List(header("Authorization", s"Bearer $AccessToken"), header("User-Agent", s"$ClientIdentifier Dropbox-Scala-SDK/1.0"))
      probe expectMsg HttpRequest(uri = expectedURI, headers = expectedHeaders)
    }

    it("should stream the file contents") {
      val probe = ioProbe
      val expectedContents = "ce n'est pas un test"

      val response = dropbox getFile (probe.ref, Root, Path, Rev)

      probe expectMsgClass classOf[HttpRequest]
      probe.reply(HttpResponse(entity = HttpEntity(`text/plain`, expectedContents), headers = List(header("x-dropbox-metadata", ContentMetadataJson))))

      val actual = await(response)
      actual._2.foldLeft("")(_ + _.asString) shouldEqual expectedContents
    }

    it("should parse the content metadata") {
      val probe = ioProbe

      val response = dropbox getFile (probe.ref, Root, Path, Rev)

      probe expectMsgClass classOf[HttpRequest]
      probe.reply(HttpResponse(headers = List(header("x-dropbox-metadata", ContentMetadataJson))))

      val actual = await(response)
      actual._1 shouldEqual Metadata
    }

    it("should propagate authorization failures") {
      pending
    }

    it("should propagate not found failures") {
      pending
    }
  }

  val Metadata = ContentMetadata("0 bytes", 0, "path", false, None, Some("rev"), false, "icon", Some(formatter.parse("Mon, 18 Jul 2011 20:13:43 +0000")), formatter.parse("Wed, 20 Apr 2011 16:20:19 +0000"), "root", "mime_type", Some(1))

  def formatter: DateFormat = new SimpleDateFormat("EEE, d MMM yyyy HH:mm:ss Z")

  val ContentMetadataJson = s"""
    {
         "size": "${Metadata.size}",
         "bytes": ${Metadata.bytes},
         "path": "${Metadata.path}",
         "is_dir": ${Metadata.is_dir},
         "rev": "${Metadata.rev.get}",
         "thumb_exists": ${Metadata.thumb_exists},
         "icon": "${Metadata.icon}",
         "modified": "${formatter.format(Metadata.modified.get)}",
         "client_mtime": "${formatter.format(Metadata.client_mtime)}",
         "root": "${Metadata.root}",
         "mime_type": "${Metadata.mime_type}",
         "revision": ${Metadata.revision.get}
    }
    """
}