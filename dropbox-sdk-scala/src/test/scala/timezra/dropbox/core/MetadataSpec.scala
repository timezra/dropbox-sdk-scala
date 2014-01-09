package timezra.dropbox.core

import org.junit.runner.RunWith
import spray.http.HttpEntity
import spray.http.HttpRequest
import spray.http.HttpResponse
import spray.http.StatusCodes
import spray.httpx.UnsuccessfulResponseException
import org.scalatest.junit.JUnitRunner
import ContentTypes.`text/javascript`
import java.util.Locale
import java.text.DateFormat
import java.text.SimpleDateFormat

@RunWith(classOf[JUnitRunner])
class MetadataSpec extends CoreSpec {

  val Root = "root"
  val Path = "test.txt"
  val FileLimit = 25000
  val Hash = "hash"
  val Rev = "test"

  val Metadata = ContentMetadata("0 bytes", 0, "path", false, None, Some("rev"), None, false, "icon", Some(formatter.parse("Mon, 18 Jul 2011 20:13:43 +0000")), Some(formatter.parse("Wed, 20 Apr 2011 16:20:19 +0000")), "root", Some("mime_type"), Some(1))
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
      "client_mtime": "${formatter.format(Metadata.client_mtime.get)}",
      "root": "${Metadata.root}",
      "mime_type": "${Metadata.mime_type.get}",
      "revision": ${Metadata.revision.get}
  }
  """
  val NotFoundFailure = """{"error": "Path not found"}"""

  describe("Metadata") {

    it("should make an http request") {
      val probe = ioProbe

      dropbox metadata (probe ref, Root, Path)

      val expectedURI = s"https://api.dropbox.com/1/metadata/$Root/$Path"
      probe expectMsg HttpRequest(uri = expectedURI, headers = List(authorizationHeader, userAgentHeader))
    }

    it("should request a specific file limit") {
      val probe = ioProbe

      dropbox metadata (probe ref, Root, Path, Some(FileLimit))

      val request = probe expectMsgClass classOf[HttpRequest]
      request.uri.query.get("file_limit").get.toInt should be(FileLimit)
    }

    it("should request a specific hash") {
      val probe = ioProbe

      dropbox metadata (probe ref, Root, Path, hash = Some(Hash))

      val request = probe expectMsgClass classOf[HttpRequest]
      request.uri.query.get("hash").get should be(Hash)
    }

    it("should request that contents be listed") {
      val probe = ioProbe

      dropbox metadata (probe ref, Root, Path, list = Some(true))

      val request = probe expectMsgClass classOf[HttpRequest]
      request.uri.query.get("list").get.toBoolean should be(true)
    }

    it("should request that deleted contents be listed") {
      val probe = ioProbe

      dropbox metadata (probe ref, Root, Path, include_deleted = Some(true))

      val request = probe expectMsgClass classOf[HttpRequest]
      request.uri.query.get("include_deleted").get.toBoolean should be(true)
    }

    it("should request a specific revision") {
      val probe = ioProbe

      dropbox metadata (probe ref, Root, Path, rev = Some(Rev))

      val request = probe expectMsgClass classOf[HttpRequest]
      request.uri.query.get("rev").get should be(Rev)
    }

    it("should request language specific text") {
      val probe = ioProbe

      implicit val locale = Some(Locale.CHINA)
      dropbox metadata (probe ref, Root, Path)

      val request = probe expectMsgClass classOf[HttpRequest]

      request.uri.query.get("locale") should be(locale.map(_.toLanguageTag))
    }
  }
}