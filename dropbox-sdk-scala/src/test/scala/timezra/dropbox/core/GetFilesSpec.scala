package timezra.dropbox.core

import java.text.DateFormat
import java.text.SimpleDateFormat
import org.junit.runner.RunWith
import spray.http.ContentTypes.`text/plain`
import spray.http.HttpEntity
import spray.http.HttpRequest
import spray.http.HttpResponse
import spray.http.StatusCodes.Unauthorized
import spray.http.StatusCodes.NotFound
import spray.httpx.UnsuccessfulResponseException
import org.scalatest.junit.JUnitRunner
import ContentTypes.`text/javascript`

@RunWith(classOf[JUnitRunner])
class GetFilesSpec extends CoreSpec {

  val Root = "root"
  val Path = "test.txt"
  val Rev = "test"

  val Metadata = ContentMetadata("0 bytes", 0, "path", false, None, Some("rev"), None, false, "icon", Some(formatter.parse("Mon, 18 Jul 2011 20:13:43 +0000")), formatter.parse("Wed, 20 Apr 2011 16:20:19 +0000"), "root", "mime_type", Some(1))
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
  val NotFoundFailure = """{"error": "File not found"}"""

  describe("Files (GET)") {
    it("should make an http request") {
      val probe = ioProbe

      dropbox getFile (probe ref, Root, Path)

      val expectedURI = s"https://api-content.dropbox.com/1/files/$Root/$Path"
      probe expectMsg HttpRequest(uri = expectedURI, headers = List(authorizationHeader, userAgentHeader))
    }

    it("should request a specific revision") {
      val probe = ioProbe

      dropbox getFile (probe ref, Root, Path, Some(Rev))

      val request = probe expectMsgClass classOf[HttpRequest]
      request.uri.query.get("rev").get should be(Rev)
    }

    it("should request a byte range") {
      val probe = ioProbe

      dropbox getFile (probe ref, path = Path, range = Some(Seq(ByteRange(Some(0), Some(5)), ByteRange(Some(10), Some(20)))))

      val request = probe expectMsgClass classOf[HttpRequest]
      request.headers should contain(header("Range", "bytes=0-5,10-20"))
    }

    it("should stream the file contents") {
      val probe = ioProbe
      val expectedContents = "ce n'est pas un test"

      val response = dropbox getFile (probe ref, path = Path)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(entity = HttpEntity(`text/plain`, expectedContents), headers = List(header("x-dropbox-metadata", ContentMetadataJson))))

      val actual = await(response)
      actual._2.foldLeft("")(_ + _.asString) should be(expectedContents)
    }

    it("should parse the content metadata") {
      val probe = ioProbe

      val response = dropbox getFile (probe ref, path = Path)

      probe expectMsgClass classOf[HttpRequest]
      probe.reply(HttpResponse(headers = List(header("x-dropbox-metadata", ContentMetadataJson))))

      val actual = await(response)
      actual._1 should be(Metadata)
    }

    it("should propagate authorization failures") {
      val probe = ioProbe

      val response = dropbox getFile (probe ref, path = Path)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(Unauthorized, HttpEntity(`text/javascript`, AuthorizationFailure)))

      val error = intercept[UnsuccessfulResponseException] { await(response) }
      error.getMessage() should be(s"Status: $Unauthorized\nBody: $AuthorizationFailure")
    }

    it("should propagate not found failures") {
      val probe = ioProbe

      val response = dropbox getFile (probe ref, path = Path)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(NotFound, HttpEntity(ContentTypes `text/javascript`, NotFoundFailure)))

      val error = intercept[UnsuccessfulResponseException] { await(response) }
      error.getMessage should be(s"Status: $NotFound\nBody: $NotFoundFailure")
    }
  }
}