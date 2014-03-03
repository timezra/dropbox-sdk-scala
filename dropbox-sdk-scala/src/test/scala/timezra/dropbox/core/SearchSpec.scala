package timezra.dropbox.core

import java.text.DateFormat
import java.text.SimpleDateFormat
import java.util.Locale
import org.junit.runner.RunWith
import ContentTypes.`text/javascript`
import spray.http.ContentTypes.`application/json`
import spray.http.HttpEntity
import spray.http.HttpRequest
import spray.http.HttpMethods.POST
import spray.http.HttpResponse
import spray.http.StatusCodes.BadRequest
import spray.http.StatusCodes.OK
import spray.http.Uri.apply
import spray.httpx.UnsuccessfulResponseException
import org.scalatest.junit.JUnitRunner
import org.scalatest.Inside
import spray.http.Uri
import spray.http.HttpData.Bytes

@RunWith(classOf[JUnitRunner])
class SearchSpec extends CoreSpec with Inside {

  val Root = "root"
  val Path = "test.txt"
  val Query = "query"
  val FileLimit = 50

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

  val SearchMetadataJson = s"[$FileMetadataJson]"

  val BadRequestFailure = s"""{"error": {"query": "Please enter a value"}}"""

  describe("Search (GET)") {

    it("should make a GET request") {
      val probe = ioProbe

      dropbox search (probe ref, Root, Some(Path), Query)

      val expectedURI = s"https://api.dropbox.com/1/search/$Root/$Path?query=$Query"
      probe expectMsg HttpRequest(uri = expectedURI, headers = List(authorizationHeader, userAgentHeader))
    }

    it("should request a specific file limit") {
      val probe = ioProbe

      dropbox search (probe ref, query = Query, file_limit = Some(FileLimit))

      val request = probe expectMsgClass classOf[HttpRequest]
      request.uri.query.get("file_limit").get.toInt should be(FileLimit)
    }

    it("should request that deleted entries be included") {
      val probe = ioProbe
      val includeDeleted = true

      dropbox search (probe ref, query = Query, include_deleted = Some(includeDeleted))

      val request = probe expectMsgClass classOf[HttpRequest]
      request.uri.query.get("include_deleted").get should be(includeDeleted.toString)
    }

    it("should request language specific text") {
      val probe = ioProbe

      implicit val locale = Some(Locale.CHINA)
      dropbox search (probe ref, query = Query)

      val request = probe expectMsgClass classOf[HttpRequest]

      request.uri.query.get("locale") should be(locale.map(_.toLanguageTag))
    }

    it("should return file content metadata") {
      val probe = ioProbe

      val response = dropbox search (probe ref, query = Query)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(OK, HttpEntity(`text/javascript`, SearchMetadataJson)))

      val revisions = await(response)

      revisions.head should be(FileMetadata)
    }

    it("should propagate bad request failures") {
      val probe = ioProbe

      val response = dropbox search (probe ref, query = Query)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(BadRequest, HttpEntity(`application/json`, BadRequestFailure)))

      val error = intercept[UnsuccessfulResponseException] { await(response) }
      error.getMessage should be(s"Status: $BadRequest\nBody: $BadRequestFailure")
    }
  }

  describe("Search (POST)") {

    it("should make a POST request") {
      val probe = ioProbe

      dropbox search (probe ref, method = POST, root = Root, path = Some(Path), query = Query)

      val request = probe expectMsgClass classOf[HttpRequest]
      inside(request) {
        case HttpRequest(method, uri, headers, _, _) ⇒
          method should be(POST)
          uri should be(Uri(s"https://api.dropbox.com/1/search/$Root/$Path"))
          headers should be(List(authorizationHeader, userAgentHeader))
      }
    }

    it("should request a specific file limit") {
      val probe = ioProbe

      dropbox search (probe ref, method = POST, query = Query, file_limit = Some(FileLimit))

      val request = probe expectMsgClass classOf[HttpRequest]
      request match {
        case HttpRequest(POST, _, _, HttpEntity.NonEmpty(_, Bytes(byteString)), _) ⇒
          byteString.utf8String should include(s"file_limit=$FileLimit")
      }
    }

    it("should request that deleted entries be included") {
      val probe = ioProbe
      val includeDeleted = true

      dropbox search (probe ref, method = POST, query = Query, include_deleted = Some(includeDeleted))

      val request = probe expectMsgClass classOf[HttpRequest]
      request match {
        case HttpRequest(POST, _, _, HttpEntity.NonEmpty(_, Bytes(byteString)), _) ⇒
          byteString.utf8String should include(s"include_deleted=$includeDeleted")
      }
    }

    it("should request language specific text") {
      val probe = ioProbe

      implicit val locale = Some(Locale.CHINA)
      dropbox search (probe ref, method = POST, query = Query)

      val request = probe expectMsgClass classOf[HttpRequest]
      request match {
        case HttpRequest(POST, _, _, HttpEntity.NonEmpty(_, Bytes(byteString)), _) ⇒
          byteString.utf8String should include(s"locale=${locale.get.toLanguageTag}")
      }
    }

    it("should return file content metadata") {
      val probe = ioProbe

      val response = dropbox search (probe ref, method = POST, query = Query)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(OK, HttpEntity(`text/javascript`, SearchMetadataJson)))

      val revisions = await(response)

      revisions.head should be(FileMetadata)
    }

    it("should propagate bad request failures") {
      val probe = ioProbe

      val response = dropbox search (probe ref, method = POST, query = Query)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(BadRequest, HttpEntity(`application/json`, BadRequestFailure)))

      val error = intercept[UnsuccessfulResponseException] { await(response) }
      error.getMessage should be(s"Status: $BadRequest\nBody: $BadRequestFailure")
    }
  }
}