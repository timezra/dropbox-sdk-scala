package timezra.dropbox.core

import java.text.DateFormat
import java.text.SimpleDateFormat
import java.util.Locale
import org.junit.runner.RunWith
import ContentTypes.`text/javascript`
import spray.http.ContentType.apply
import spray.http.ContentTypes.`application/json`
import spray.http.HttpEntity
import spray.http.HttpRequest
import spray.http.HttpMethods.POST
import spray.http.HttpResponse
import spray.http.StatusCodes.BadRequest
import spray.http.StatusCodes.NotFound
import spray.http.StatusCodes.OK
import spray.http.Uri.apply
import spray.httpx.UnsuccessfulResponseException
import org.scalatest.junit.JUnitRunner
import org.scalatest.Inside
import spray.http.Uri
import spray.http.HttpData.Bytes

@RunWith(classOf[JUnitRunner])
class SharesSpec extends CoreSpec with Inside {

  val Root = "root"
  val Path = "test.txt"
  val Url = Uri("https://db.tt/testurl")

  val Metadata = SharesMetadata(Url, formatter.parse("Tue, 01 Jan 2030 00:00:00 +0000"))
  def formatter: DateFormat = new SimpleDateFormat("EEE, d MMM yyyy HH:mm:ss Z")

  val ShareMetadataJson = s"""
  {
      "url": "${Metadata.url}",
      "expires": "${formatter.format(Metadata.expires)}"
  }
  """

  val BadRequestFailure = s"""{"error": {"path": "The root path is not allowed."}}"""
  val NotFoundFailure = s"""{"error": "Path not found"}"""

  describe("Shares") {

    it("should make an http request") {
      val probe = ioProbe

      dropbox shares (probe ref, Root, Path)

      val request = probe expectMsgClass classOf[HttpRequest]
      inside(request) {
        case HttpRequest(method, uri, headers, _, _) ⇒
          method should be(POST)
          uri should be(Uri(s"https://api.dropbox.com/1/shares/$Root/$Path"))
          headers should be(List(authorizationHeader, userAgentHeader))
      }
    }

    it("should request a short url") {
      val probe = ioProbe
      val shortUrl = true

      dropbox shares (probe ref, path = Path, short_url = Some(shortUrl))

      val request = probe expectMsgClass classOf[HttpRequest]
      request match {
        case HttpRequest(POST, _, _, HttpEntity.NonEmpty(_, Bytes(byteString)), _) ⇒
          byteString.utf8String should include(s"short_url=$shortUrl")
      }
    }

    it("should request language specific text") {
      val probe = ioProbe

      implicit val locale = Some(Locale.CHINA)
      dropbox shares (probe ref, path = Path)

      val request = probe expectMsgClass classOf[HttpRequest]
      request match {
        case HttpRequest(POST, _, _, HttpEntity.NonEmpty(_, Bytes(byteString)), _) ⇒
          byteString.utf8String should include(s"locale=${locale.get.toLanguageTag}")
      }
    }

    it("should return share metadata") {
      val probe = ioProbe

      val response = dropbox shares (probe ref, path = Path)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(OK, HttpEntity(`text/javascript`, ShareMetadataJson)))

      val metadata = await(response)

      metadata should be(Metadata)
    }

    it("should propagate bad request failures") {
      val probe = ioProbe

      val response = dropbox shares (probe ref, path = Path)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(BadRequest, HttpEntity(`application/json`, BadRequestFailure)))

      val error = intercept[UnsuccessfulResponseException] { await(response) }
      error.getMessage should be(s"Status: $BadRequest\nBody: $BadRequestFailure")
    }

    it("should propagate not found failures") {
      val probe = ioProbe

      val response = dropbox shares (probe ref, path = Path)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(NotFound, HttpEntity(`application/json`, NotFoundFailure)))

      val error = intercept[UnsuccessfulResponseException] { await(response) }
      error.getMessage should be(s"Status: $NotFound\nBody: $NotFoundFailure")
    }
  }
}