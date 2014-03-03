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
import spray.http.StatusCodes.NotFound
import spray.http.StatusCodes.OK
import spray.http.Uri
import spray.httpx.UnsuccessfulResponseException
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MediaSpec extends CoreSpec with Inside {

  val Root = "root"
  val Path = "test.txt"
  val Url = Uri("https://dl.dropboxusercontent.com/1/view/something/Apps/my-app/test.txt")

  val MediaLinkWithExpiry = LinkWithExpiry(Url, formatter.parse("Mon, 03 Mar 2014 10:51:47 +0000"))
  def formatter: DateFormat = new SimpleDateFormat("EEE, d MMM yyyy HH:mm:ss Z")

  val MediaLinkWithExpiryJson = s"""
  {
      "url": "${MediaLinkWithExpiry.url}",
      "expires": "${formatter.format(MediaLinkWithExpiry.expires)}"
  }
  """

  val BadRequestFailure = s"""{"error": {"path": "The root path is not allowed."}}"""
  val NotFoundFailure = s"""{"error": "Path not found"}"""

  describe("Shares") {

    it("should make an http request") {
      val probe = ioProbe

      dropbox media (probe ref, Root, Path)

      val request = probe expectMsgClass classOf[HttpRequest]
      inside(request) {
        case HttpRequest(method, uri, headers, _, _) ⇒
          method should be(POST)
          uri should be(Uri(s"https://api.dropbox.com/1/media/$Root/$Path"))
          headers should be(List(authorizationHeader, userAgentHeader))
      }
    }

    it("should request language specific text") {
      val probe = ioProbe

      implicit val locale = Some(Locale.CHINA)
      dropbox media (probe ref, path = Path)

      val request = probe expectMsgClass classOf[HttpRequest]
      request match {
        case HttpRequest(POST, _, _, HttpEntity.NonEmpty(_, Bytes(byteString)), _) ⇒
          byteString.utf8String should include(s"locale=${locale.get.toLanguageTag}")
      }
    }

    it("should return an expiring link") {
      val probe = ioProbe

      val response = dropbox media (probe ref, path = Path)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(OK, HttpEntity(`text/javascript`, MediaLinkWithExpiryJson)))

      val mediaLinkWithExpiry = await(response)

      mediaLinkWithExpiry should be(MediaLinkWithExpiry)
    }

    it("should propagate bad request failures") {
      val probe = ioProbe

      val response = dropbox media (probe ref, path = Path)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(BadRequest, HttpEntity(`application/json`, BadRequestFailure)))

      val error = intercept[UnsuccessfulResponseException] { await(response) }
      error.getMessage should be(s"Status: $BadRequest\nBody: $BadRequestFailure")
    }

    it("should propagate not found failures") {
      val probe = ioProbe

      val response = dropbox media (probe ref, path = Path)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(NotFound, HttpEntity(`application/json`, NotFoundFailure)))

      val error = intercept[UnsuccessfulResponseException] { await(response) }
      error.getMessage should be(s"Status: $NotFound\nBody: $NotFoundFailure")
    }
  }
}