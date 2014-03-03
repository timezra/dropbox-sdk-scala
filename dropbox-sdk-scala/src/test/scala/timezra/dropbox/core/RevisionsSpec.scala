package timezra.dropbox.core

import org.junit.runner.RunWith
import spray.http.HttpEntity
import spray.http.HttpRequest
import spray.http.HttpResponse
import spray.http.StatusCodes.NotFound
import spray.http.StatusCodes.OK
import spray.httpx.UnsuccessfulResponseException
import org.scalatest.junit.JUnitRunner
import ContentTypes.`text/javascript`
import java.util.Locale
import java.text.DateFormat
import java.text.SimpleDateFormat

@RunWith(classOf[JUnitRunner])
class RevisionsSpec extends CoreSpec {

  val Root = "root"
  val Path = "test.txt"
  val RevLimit = 10000

  val FileMetadata = ContentMetadata("10 bytes", 10, s"/$Path", false, None, Some("rev"), None, false, "fileIcon", Some(formatter.parse("Mon, 18 Jul 2011 20:13:43 +0000")), Some(formatter.parse("Wed, 20 Apr 2011 16:20:19 +0000")), "root", Some("mime_type"), Some(1), None)
  def formatter: DateFormat = new SimpleDateFormat("EEE, d MMM yyyy HH:mm:ss Z")

  val FileMetadataJson = s"""
  {
      "size": "${FileMetadata.size}",
      "bytes": ${FileMetadata.bytes},
      "path": "${FileMetadata.path}",
      "is_dir": ${FileMetadata.is_dir},
      "rev": "${FileMetadata.rev.get}",
      "thumb_exists": ${FileMetadata.thumb_exists},
      "icon": "${FileMetadata.icon}",
      "modified": "${formatter.format(FileMetadata.modified.get)}",
      "client_mtime": "${formatter.format(FileMetadata.client_mtime.get)}",
      "root": "${FileMetadata.root}",
      "mime_type": "${FileMetadata.mime_type.get}",
      "revision": ${FileMetadata.revision.get}
  }
  """

  val RevMetadataJson = s"[$FileMetadataJson]"

  val NotFoundFailure = """{"error": "Path not found"}"""

  describe("Revisions") {

    it("should make an http request") {
      val probe = ioProbe

      dropbox revisions (probe ref, Root, Path)

      val expectedURI = s"https://api.dropbox.com/1/revisions/$Root/$Path"
      probe expectMsg HttpRequest(uri = expectedURI, headers = List(authorizationHeader, userAgentHeader))
    }

    it("should request a specific rev limit") {
      val probe = ioProbe

      dropbox revisions (probe ref, Root, Path, Some(RevLimit))

      val request = probe expectMsgClass classOf[HttpRequest]
      request.uri.query.get("rev_limit").get.toInt should be(RevLimit)
    }

    it("should request language specific text") {
      val probe = ioProbe

      implicit val locale = Some(Locale.CHINA)
      dropbox revisions (probe ref, Root, Path)

      val request = probe expectMsgClass classOf[HttpRequest]

      request.uri.query.get("locale") should be(locale.map(_.toLanguageTag))
    }

    it("should return file content metadata") {
      val probe = ioProbe

      val response = dropbox revisions (probe ref, Root, Path)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(OK, HttpEntity(ContentTypes `text/javascript`, RevMetadataJson)))

      val revisions = await(response)

      revisions(0) should be(FileMetadata)
    }

    it("should propagate not found failures") {
      val probe = ioProbe

      val response = dropbox revisions (probe ref, path = Path)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(NotFound, HttpEntity(`text/javascript`, NotFoundFailure)))

      val error = intercept[UnsuccessfulResponseException] { await(response) }
      error.getMessage should be(s"Status: $NotFound\nBody: $NotFoundFailure")
    }
  }
}