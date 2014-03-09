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
import spray.http.StatusCodes.Forbidden
import spray.http.StatusCodes.OK
import spray.http.Uri
import spray.httpx.UnsuccessfulResponseException
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CreateFolderSpec extends CoreSpec with Inside {

  val Root = "root"
  val Path = "path"

  def formatter: DateFormat = new SimpleDateFormat("EEE, d MMM yyyy HH:mm:ss Z")
  val Metadata = ContentMetadata("0 bytes", 0, s"/$Path", true, None, Some("rev"), None, false, "icon", Some(formatter.parse("Mon, 18 Jul 2011 20:13:43 +0000")), Some(formatter.parse("Wed, 20 Apr 2011 16:20:19 +0000")), Root, None, Some(1), None)
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
      "revision": ${Metadata.revision.get}
  }
  """

  val ForbiddenFailure = s"""{"error": "Cannot create folder because a file or folder already exists at the path"}"""
  val BadRequestFailure = s"""{"error": "Expected parameter, got nothing"}"""

  describe("CreateFolder") {

    it("should make an http request") {
      val probe = ioProbe

      dropbox create_folder (probe ref, Root, Path)

      val request = probe expectMsgClass classOf[HttpRequest]

      request match {
        case HttpRequest(method, uri, headers, _, _) ⇒
          method should be(POST)
          uri should be(Uri("https://api.dropbox.com/1/fileops/create_folder"))
          headers should (contain(authorizationHeader) and contain(userAgentHeader))
      }
    }

    it("should create a folder relative to a root") {
      val probe = ioProbe

      dropbox create_folder (probe ref, Root, Path)

      val request = probe expectMsgClass classOf[HttpRequest]
      request match {
        case HttpRequest(POST, _, _, HttpEntity.NonEmpty(_, Bytes(byteString)), _) ⇒
          byteString.utf8String should include(s"root=$Root")
      }
    }

    it("should create a folder at a location") {
      val probe = ioProbe

      dropbox create_folder (probe ref, path = Path)

      val request = probe expectMsgClass classOf[HttpRequest]
      request match {
        case HttpRequest(POST, _, _, HttpEntity.NonEmpty(_, Bytes(byteString)), _) ⇒
          byteString.utf8String should include(s"path=$Path")
      }
    }

    it("should request language specific text") {
      val probe = ioProbe

      implicit val locale = Some(Locale.CHINA)
      dropbox create_folder (probe ref, path = Path)

      val request = probe expectMsgClass classOf[HttpRequest]
      request match {
        case HttpRequest(POST, _, _, HttpEntity.NonEmpty(_, Bytes(byteString)), _) ⇒
          byteString.utf8String should include(s"locale=${locale.map(_.toLanguageTag).get}")
      }
    }

    it("should return metadata for the created folder") {
      val probe = ioProbe

      val response = dropbox create_folder (probe ref, path = Path)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(OK, HttpEntity(`text/javascript`, ContentMetadataJson)))

      val metadata = await(response)

      metadata should be(Metadata)
    }

    it("should propagate forbidden failures") {
      val probe = ioProbe

      val response = dropbox create_folder (probe ref, path = Path)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(Forbidden, HttpEntity(`application/json`, ForbiddenFailure)))

      val error = intercept[UnsuccessfulResponseException] { await(response) }
      error.getMessage should be(s"Status: $Forbidden\nBody: $ForbiddenFailure")
    }

    it("should propagate bad request failures") {
      val probe = ioProbe

      val response = dropbox create_folder (probe ref, path = Path)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(BadRequest, HttpEntity(`application/json`, BadRequestFailure)))

      val error = intercept[UnsuccessfulResponseException] { await(response) }
      error.getMessage should be(s"Status: $BadRequest\nBody: $BadRequestFailure")
    }
  }
}