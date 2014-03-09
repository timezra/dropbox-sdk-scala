package timezra.dropbox.core

import java.text.DateFormat
import java.text.SimpleDateFormat
import java.util.Locale
import org.junit.runner.RunWith
import org.scalatest.Inside
import ContentTypes.`text/javascript`
import spray.http.ContentTypes.`application/json`
import spray.http.HttpData.Bytes
import spray.http.HttpEntity
import spray.http.HttpMethods.POST
import spray.http.HttpRequest
import spray.http.HttpResponse
import spray.http.StatusCodes.BadRequest
import spray.http.StatusCodes.NotAcceptable
import spray.http.StatusCodes.NotFound
import spray.http.StatusCodes.OK
import spray.http.Uri
import spray.httpx.UnsuccessfulResponseException
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class DeleteSpec extends CoreSpec with Inside {

  val Root = "root"
  val Path = "test.txt"

  def formatter: DateFormat = new SimpleDateFormat("EEE, d MMM yyyy HH:mm:ss Z")
  val Metadata = ContentMetadata("0 bytes", 0, s"/$Path", false, Some(true), Some("rev"), None, false, "icon", Some(formatter.parse("Mon, 18 Jul 2011 20:13:43 +0000")), Some(formatter.parse("Wed, 20 Apr 2011 16:20:19 +0000")), Root, Some("mime_type"), Some(1), None)
  val ContentMetadataJson = s"""
  {
      "size": "${Metadata.size}",
      "bytes": ${Metadata.bytes},
      "path": "${Metadata.path}",
      "is_dir": ${Metadata.is_dir},
      "is_deleted": ${Metadata.is_deleted.get},
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

  val NotFoundFailure = s"""{"error": "Path not found."}"""
  val NotAcceptableFailure = s"""{"error": "Too many files"}"""
  val BadRequestFailure = s"""{"error": "Must send either a from_path or a from_copy_ref"}"""

  describe("Delete") {

    it("should make an http request") {
      val probe = ioProbe

      dropbox delete (probe ref, Root, Path)

      val request = probe expectMsgClass classOf[HttpRequest]

      request match {
        case HttpRequest(method, uri, headers, _, _) ⇒
          method should be(POST)
          uri should be(Uri("https://api.dropbox.com/1/fileops/delete"))
          headers should (contain(authorizationHeader) and contain(userAgentHeader))
      }
    }

    it("should delete a file relative to a root") {
      val probe = ioProbe

      dropbox delete (probe ref, Root, Path)

      val request = probe expectMsgClass classOf[HttpRequest]
      request match {
        case HttpRequest(POST, _, _, HttpEntity.NonEmpty(_, Bytes(byteString)), _) ⇒
          byteString.utf8String should include(s"root=$Root")
      }
    }

    it("should delete a file from a location") {
      val probe = ioProbe

      dropbox delete (probe ref, path = Path)

      val request = probe expectMsgClass classOf[HttpRequest]
      request match {
        case HttpRequest(POST, _, _, HttpEntity.NonEmpty(_, Bytes(byteString)), _) ⇒
          byteString.utf8String should include(s"path=$Path")
      }
    }

    it("should request language specific text") {
      val probe = ioProbe

      implicit val locale = Some(Locale.CHINA)
      dropbox delete (probe ref, path = Path)

      val request = probe expectMsgClass classOf[HttpRequest]
      request match {
        case HttpRequest(POST, _, _, HttpEntity.NonEmpty(_, Bytes(byteString)), _) ⇒
          byteString.utf8String should include(s"locale=${locale.map(_.toLanguageTag).get}")
      }
    }

    it("should return metadata for the deleted file") {
      val probe = ioProbe

      val response = dropbox delete (probe ref, path = Path)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(OK, HttpEntity(`text/javascript`, ContentMetadataJson)))

      val metadata = await(response)

      metadata should be(Metadata)
    }

    it("should propagate not found failures") {
      val probe = ioProbe

      val response = dropbox delete (probe ref, path = Path)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(NotFound, HttpEntity(`application/json`, NotFoundFailure)))

      val error = intercept[UnsuccessfulResponseException] { await(response) }
      error.getMessage should be(s"Status: $NotFound\nBody: $NotFoundFailure")
    }

    it("should propagate too many files failures") {
      val probe = ioProbe

      val response = dropbox delete (probe ref, path = Path)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(NotAcceptable, HttpEntity(`application/json`, NotAcceptableFailure)))

      val error = intercept[UnsuccessfulResponseException] { await(response) }
      error.getMessage should be(s"Status: $NotAcceptable\nBody: $NotAcceptableFailure")
    }

    it("should propagate bad request failures") {
      val probe = ioProbe

      val response = dropbox delete (probe ref, path = Path)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(BadRequest, HttpEntity(`application/json`, BadRequestFailure)))

      val error = intercept[UnsuccessfulResponseException] { await(response) }
      error.getMessage should be(s"Status: $BadRequest\nBody: $BadRequestFailure")
    }
  }
}