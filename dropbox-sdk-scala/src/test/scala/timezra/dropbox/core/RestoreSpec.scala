package timezra.dropbox.core

import java.text.DateFormat
import java.text.SimpleDateFormat
import java.util.Locale
import org.junit.runner.RunWith
import org.scalatest.Inside
import ContentTypes.`text/javascript`
import spray.http.ContentType.apply
import spray.http.HttpData.Bytes
import spray.http.HttpEntity
import spray.http.HttpMethods.POST
import spray.http.HttpRequest
import spray.http.HttpResponse
import spray.http.StatusCodes.BadRequest
import spray.http.StatusCodes.NotFound
import spray.http.StatusCodes.OK
import spray.http.Uri
import spray.httpx.UnsuccessfulResponseException
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RestoreSpec extends CoreSpec with Inside {

  val Root = "root"
  val Path = "test.txt"
  val Rev = "rev"

  val FileMetadata = ContentMetadata("10 bytes", 10, s"/$Path", false, None, Some("A new rev"), None, false, "fileIcon", Some(formatter.parse("Mon, 18 Jul 2011 20:13:43 +0000")), Some(formatter.parse("Wed, 20 Apr 2011 16:20:19 +0000")), "root", Some("mime_type"), Some(2), None)
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

  val NotFoundFailure = """{"error": "Path not found"}"""

  val BadRequestFailure = """{"error": "Unable to restore to rev"}"""

  describe("Revisions") {

    it("should make an http request") {
      val probe = ioProbe

      dropbox restore (probe ref, Root, Path, Rev)

      val request = probe expectMsgClass classOf[HttpRequest]
      inside(request) {
        case HttpRequest(method, uri, headers, _, _) ⇒
          method should be(POST)
          uri should be(Uri(s"https://api.dropbox.com/1/restore/$Root/$Path"))
          headers should be(List(authorizationHeader, userAgentHeader))
      }
    }

    it("should restore a specific rev") {
      val probe = ioProbe

      dropbox restore (probe ref, Root, Path, Rev)

      val request = probe expectMsgClass classOf[HttpRequest]

      request match {
        case HttpRequest(POST, _, _, HttpEntity.NonEmpty(_, Bytes(byteString)), _) ⇒
          byteString.utf8String should be(s"rev=$Rev")
      }
    }

    it("should request language specific text") {
      val probe = ioProbe

      implicit val locale = Some(Locale.CHINA)
      dropbox restore (probe ref, Root, Path, Rev)

      val request = probe expectMsgClass classOf[HttpRequest]

      request match {
        case HttpRequest(POST, _, _, HttpEntity.NonEmpty(_, Bytes(byteString)), _) ⇒
          byteString.utf8String should include(s"locale=${locale.get.toLanguageTag}")
      }
    }

    it("should return file content metadata") {
      val probe = ioProbe

      val response = dropbox restore (probe ref, Root, Path, Rev)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(OK, HttpEntity(ContentTypes `text/javascript`, FileMetadataJson)))

      await(response) should be(FileMetadata)
    }

    it("should propagate not found failures") {
      val probe = ioProbe

      val response = dropbox restore (probe ref, Root, Path, Rev)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(NotFound, HttpEntity(`text/javascript`, NotFoundFailure)))

      val error = intercept[UnsuccessfulResponseException] { await(response) }
      error.getMessage should be(s"Status: $NotFound\nBody: $NotFoundFailure")
    }

    it("should propagate unable to restore failures") {
      val probe = ioProbe

      val response = dropbox restore (probe ref, Root, Path, Rev)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(BadRequest, HttpEntity(`text/javascript`, BadRequestFailure)))

      val error = intercept[UnsuccessfulResponseException] { await(response) }
      error.getMessage should be(s"Status: $BadRequest\nBody: $BadRequestFailure")
    }
  }
}