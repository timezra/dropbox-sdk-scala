package timezra.dropbox.core

import java.text.DateFormat
import java.text.SimpleDateFormat
import java.util.Locale
import org.junit.runner.RunWith
import ContentTypes.`text/javascript`
import Implicits.string2Enum
import spray.http.ContentTypes.`application/json`
import spray.http.ContentTypes.`application/octet-stream`
import spray.http.HttpEntity
import spray.http.HttpMethods
import spray.http.HttpProtocols.`HTTP/1.1`
import spray.http.HttpRequest
import spray.http.HttpResponse
import spray.http.StatusCodes.BadRequest
import spray.http.StatusCodes.NotFound
import spray.http.Uri
import spray.httpx.UnsuccessfulResponseException
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ChunkedUploadSpec extends CoreSpec {

  val Root = "root"
  val Path = "test.txt"
  val UploadId = "uploadId"
  val Offset = 10

  def formatter: DateFormat = new SimpleDateFormat("EEE, d MMM yyyy HH:mm:ss Z")

  val ChunkedUploadData = UploadWithExpiry(UploadId, Offset, formatter.parse("Tue, 01 Jan 2030 00:00:00 +0000"))
  val ChunkedUploadResponse = s"""
  {
      "expires": "${formatter.format(ChunkedUploadData.expires)}",
      "upload_id": "${ChunkedUploadData.upload_id}",
      "offset": ${ChunkedUploadData.offset}
  }
  """

  val CommitChunkedUploadMetadata = ContentMetadata("0 bytes", 0, "path", false, None, Some("rev"), None, false, "icon", Some(formatter.parse("Mon, 18 Jul 2011 20:13:43 +0000")), Some(formatter.parse("Wed, 20 Apr 2011 16:20:19 +0000")), "root", Some("text/plain"), Some(1), None)
  val CommitChunkedUploadResponse = s"""
  {
      "size": "${CommitChunkedUploadMetadata.size}",
      "bytes": ${CommitChunkedUploadMetadata.bytes},
      "path": "${CommitChunkedUploadMetadata.path}",
      "is_dir": ${CommitChunkedUploadMetadata.is_dir},
      "rev": "${CommitChunkedUploadMetadata.rev.get}",
      "thumb_exists": ${CommitChunkedUploadMetadata.thumb_exists},
      "icon": "${CommitChunkedUploadMetadata.icon}",
      "modified": "${formatter.format(CommitChunkedUploadMetadata.modified.get)}",
      "client_mtime": "${formatter.format(CommitChunkedUploadMetadata.client_mtime.get)}",
      "root": "${CommitChunkedUploadMetadata.root}",
      "mime_type": "${CommitChunkedUploadMetadata.mime_type.get}",
      "revision": ${CommitChunkedUploadMetadata.revision.get}
  }
  """
  val NotFoundFailure = """{"error": "No upload with upload_id found."}"""

  val BadRequestFailure = s"""
  {
      "expires": "${formatter.format(ChunkedUploadData.expires)}",
      "upload_id": "${ChunkedUploadData.upload_id}",
      "offset": ${ChunkedUploadData.offset},
      "error": "Submitted input out of alignment"
  }
  """

  import Implicits._
  describe("Chunked Upload") {
    it("should make an http request") {
      val probe = ioProbe
      val contents = "ce n'est pas un test"

      dropbox chunked_upload (probe ref, contents = contents)

      val request = probe expectMsgClass classOf[HttpRequest]

      request match {
        case HttpRequest(HttpMethods.PUT, uri, headers, HttpEntity.NonEmpty(`application/octet-stream`, data), `HTTP/1.1`) ⇒
          uri should be(Uri(s"https://api-content.dropbox.com/1/chunked_upload"))
          headers should (contain(authorizationHeader) and contain(userAgentHeader))
          data.asString should be(contents)
      }
    }

    it("should set the upload_id and offset") {
      val probe = ioProbe
      val contents = "ce n'est pas un test"

      dropbox chunked_upload (probe ref, contents = contents, idAndOffset = Some(UploadId, Offset))

      val request = probe expectMsgClass classOf[HttpRequest]
      request.uri.query.get("upload_id").get should be(UploadId)
      request.uri.query.get("offset").get.toLong should be(Offset)
    }

    it("should return upload metadata") {
      val probe = ioProbe
      val contents = "ce n'est pas un test"

      val response = dropbox chunked_upload (probe ref, contents = contents)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(entity = HttpEntity(`text/javascript`, ChunkedUploadResponse)))

      await(response) should be(ChunkedUploadData)
    }

    it("should propagate not found failures") {
      val probe = ioProbe
      val contents = "ce n'est pas un test"

      val response = dropbox chunked_upload (probe ref, contents = contents, idAndOffset = Some(UploadId, Offset))

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(NotFound, HttpEntity(`application/json`, NotFoundFailure)))

      val error = intercept[UnsuccessfulResponseException] { await(response) }
      error.getMessage() should be(s"Status: $NotFound\nBody: $NotFoundFailure")
    }

    it("should propagate bad request failures") {
      val probe = ioProbe
      val contents = "ce n'est pas un test"

      val response = dropbox chunked_upload (probe ref, contents = contents, idAndOffset = Some(UploadId, Offset))

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(BadRequest, HttpEntity(`application/json`, BadRequestFailure)))

      val error = intercept[UnsuccessfulResponseException] { await(response) }
      error.getMessage() should be(s"Status: $BadRequest\nBody: $BadRequestFailure")
    }
  }

  describe("Commit Chunked Upload") {
    it("should make an http request") {
      val probe = ioProbe

      dropbox commit_chunked_upload (probe ref, root = Root, path = Path, upload_id = UploadId)

      val request = probe expectMsgClass classOf[HttpRequest]

      request match {
        case HttpRequest(HttpMethods.POST, uri, headers, HttpEntity.Empty, `HTTP/1.1`) ⇒
          uri should be(Uri(s"https://api-content.dropbox.com/1/commit_chunked_upload/$Root/$Path?upload_id=$UploadId"))
          headers should (contain(authorizationHeader) and contain(userAgentHeader))
      }
    }

    it("should return upload metadata") {
      val probe = ioProbe
      val contents = "ce n'est pas un test"

      val response = dropbox commit_chunked_upload (probe ref, path = Path, upload_id = UploadId)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(entity = HttpEntity(`text/javascript`, CommitChunkedUploadResponse)))

      await(response) should be(CommitChunkedUploadMetadata)
    }

    it("should specify a parent revision") {
      val probe = ioProbe
      val parentRev = "1"

      val response = dropbox commit_chunked_upload (probe ref, path = Path, upload_id = UploadId, parent_rev = Some(parentRev))

      val request = probe expectMsgClass classOf[HttpRequest]

      request.uri.query.get("parent_rev").get should be(parentRev)
    }

    it("should specify whether to overwrite an existing file") {
      val probe = ioProbe
      val overwrite = false

      val response = dropbox commit_chunked_upload (probe ref, path = Path, upload_id = UploadId, overwrite = Some(overwrite))

      val request = probe expectMsgClass classOf[HttpRequest]

      request.uri.query.get("overwrite").get.toBoolean should be(overwrite)
    }

    it("should ask for language specific text") {
      val probe = ioProbe

      implicit val locale = Some(Locale.CHINA)
      val response = dropbox commit_chunked_upload (probe ref, path = Path, upload_id = UploadId)

      val request = probe expectMsgClass classOf[HttpRequest]

      request.uri.query.get("locale") should be(locale.map(_.toLanguageTag))
    }

    it("should propagate not found failures") {
      val probe = ioProbe
      val contents = "ce n'est pas un test"

      val response = dropbox commit_chunked_upload (probe ref, path = Path, upload_id = UploadId)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(NotFound, HttpEntity(`application/json`, NotFoundFailure)))

      val error = intercept[UnsuccessfulResponseException] { await(response) }
      error.getMessage() should be(s"Status: $NotFound\nBody: $NotFoundFailure")
    }

    it("should propagate bad request failures") {
      val probe = ioProbe
      val contents = "ce n'est pas un test"

      val response = dropbox commit_chunked_upload (probe ref, path = Path, upload_id = UploadId)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(BadRequest, HttpEntity(`application/json`, BadRequestFailure)))

      val error = intercept[UnsuccessfulResponseException] { await(response) }
      error.getMessage() should be(s"Status: $BadRequest\nBody: $BadRequestFailure")
    }
  }
}