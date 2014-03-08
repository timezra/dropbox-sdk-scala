package timezra.dropbox.core

import java.text.DateFormat
import java.text.SimpleDateFormat
import scala.collection.mutable.ArrayBuffer
import org.junit.runner.RunWith
import ContentTypes.`image/jpeg`
import ContentTypes.`image/png`
import ContentTypes.`text/javascript`
import spray.http.HttpEntity
import spray.http.HttpRequest
import spray.http.HttpResponse
import spray.http.StatusCodes.BadRequest
import spray.http.StatusCodes.NotFound
import spray.http.StatusCodes.UnsupportedMediaType
import spray.httpx.UnsuccessfulResponseException
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ThumbnailsSpec extends CoreSpec {

  val Root = "root"
  val Path = "test.png"
  val ThumbnailFormat = Format.jpeg
  val ThumbnailSize = Size.m
  val JPEG = Array[Byte](15, 15, 13, 8, 15, 15, 14, 0, 0, 0, 1, 0, 4, 10, 4, 6, 4, 9, 4, 6, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 4, 8, 0, 0, 4, 8, 0, 0, 0, 0, 15, 15, 13, 11, 0, 0, 4, 3, 0, 0, 0, 3, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 3, 0, 2, 0, 2, 0, 2, 0, 3, 0, 3, 0, 3, 0, 3, 0, 4, 0, 6, 0, 4, 0, 4, 0, 4, 0, 4, 0, 4, 0, 8, 0, 6, 0, 6, 0, 5, 0, 6, 0, 9, 0, 8, 0, 10, 0, 10, 0, 9, 0, 8, 0, 9, 0, 9, 0, 10, 0, 12, 0, 15, 0, 12, 0, 10, 0, 11, 0, 14, 0, 11, 0, 9, 0, 9, 0, 13, 1, 1, 0, 13, 0, 14, 0, 15, 1, 0, 1, 0, 1, 1, 1, 0, 0, 10, 0, 12, 1, 2, 1, 3, 1, 2, 1, 0, 1, 3, 0, 15, 1, 0, 1, 0, 1, 0, 15, 15, 12, 9, 0, 0, 0, 11, 0, 8, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 15, 15, 12, 12, 0, 0, 0, 6, 0, 0, 1, 0, 1, 0, 0, 5, 15, 15, 13, 10, 0, 0, 0, 8, 0, 1, 0, 1, 0, 0, 0, 0, 3, 15, 0, 0, 13, 2, 12, 15, 2, 0, 15, 15, 13, 9)
  val PNG = Array[Byte](8, 9, 80, 78, 71, 13, 10, 26, 10)

  val Metadata = ContentMetadata("8.3 KB", 8545, s"/$Path", false, None, Some("rev"), None, true, "icon", Some(formatter.parse("Mon, 18 Jul 2011 20:13:43 +0000")), Some(formatter.parse("Wed, 20 Apr 2011 16:20:19 +0000")), Root, Some("mime_type"), Some(1), None)
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

  val NotFoundFailure = """{"error": "No thumbnail available"}"""
  val BadRequestFailure = """{"error": "parameter is invalid. Expected: something else"}"""
  val UnsupportedMediaTypeFailure = """{"error": "image is invalid."}"""

  describe("Thumbnails") {
    it("should make an http request") {
      val probe = ioProbe

      dropbox thumbnails (probe ref, Root, Path)

      val expectedURI = s"https://api-content.dropbox.com/1/thumbnails/$Root/$Path"
      probe expectMsg HttpRequest(uri = expectedURI, headers = List(authorizationHeader, userAgentHeader))
    }

    it("should request a specific format") {
      val probe = ioProbe

      dropbox thumbnails (probe ref, path = Path, format = Some(ThumbnailFormat))

      val request = probe expectMsgClass classOf[HttpRequest]
      request.uri.query.get("format").get should be(ThumbnailFormat.toString)
    }

    it("should request a size") {
      val probe = ioProbe

      dropbox thumbnails (probe ref, path = Path, size = Some(ThumbnailSize))

      val request = probe expectMsgClass classOf[HttpRequest]
      request.uri.query.get("size").get should be(ThumbnailSize.toString)
    }

    it("should stream the png thumbnail contents") {
      val probe = ioProbe

      val expectedContents = PNG

      val response = dropbox getFile (probe ref, path = Path)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(entity = HttpEntity(`image/png`, expectedContents), headers = List(header("x-dropbox-metadata", ContentMetadataJson))))

      val actual = await(response)
      actual._2.foldLeft(new ArrayBuffer[Byte]())(_ ++= _.toByteArray) should be(expectedContents)
    }

    it("should stream the jpeg thumbnail contents") {
      val probe = ioProbe

      val expectedContents = JPEG

      val response = dropbox getFile (probe ref, path = Path)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(entity = HttpEntity(`image/jpeg`, expectedContents), headers = List(header("x-dropbox-metadata", ContentMetadataJson))))

      val actual = await(response)
      actual._2.foldLeft(new ArrayBuffer[Byte]())(_ ++= _.toByteArray) should be(expectedContents)
    }

    it("should parse the content metadata") {
      val probe = ioProbe

      val response = dropbox getFile (probe ref, path = Path)

      probe expectMsgClass classOf[HttpRequest]
      probe.reply(HttpResponse(headers = List(header("x-dropbox-metadata", ContentMetadataJson))))

      val actual = await(response)
      actual._1 should be(Metadata)
    }

    it("should propagate not found failures") {
      val probe = ioProbe

      val response = dropbox getFile (probe ref, path = Path)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(NotFound, HttpEntity(`text/javascript`, NotFoundFailure)))

      val error = intercept[UnsuccessfulResponseException] { await(response) }
      error.getMessage should be(s"Status: $NotFound\nBody: $NotFoundFailure")
    }

    it("should propagate bad request failures") {
      val probe = ioProbe

      val response = dropbox getFile (probe ref, path = Path)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(BadRequest, HttpEntity(`text/javascript`, BadRequestFailure)))

      val error = intercept[UnsuccessfulResponseException] { await(response) }
      error.getMessage should be(s"Status: $BadRequest\nBody: $BadRequestFailure")
    }

    it("should propagate unsupported media type failures") {
      val probe = ioProbe

      val response = dropbox getFile (probe ref, path = Path)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(UnsupportedMediaType, HttpEntity(`text/javascript`, UnsupportedMediaTypeFailure)))

      val error = intercept[UnsuccessfulResponseException] { await(response) }
      error.getMessage should be(s"Status: $UnsupportedMediaType\nBody: $UnsupportedMediaTypeFailure")
    }
  }
}