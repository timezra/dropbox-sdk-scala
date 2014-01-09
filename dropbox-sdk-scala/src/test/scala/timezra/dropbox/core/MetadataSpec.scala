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
  }
}