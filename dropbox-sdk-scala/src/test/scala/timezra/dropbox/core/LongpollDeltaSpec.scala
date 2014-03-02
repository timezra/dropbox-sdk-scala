package timezra.dropbox.core

import org.junit.runner.RunWith
import spray.http.ContentType.apply
import spray.http.ContentTypes.`text/plain`
import spray.http.HttpEntity
import spray.http.HttpRequest
import spray.http.HttpResponse
import spray.http.StatusCodes.BadRequest
import spray.http.StatusCodes.OK
import spray.http.Uri.apply
import spray.httpx.UnsuccessfulResponseException
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class LongpollDeltaSpec extends CoreSpec {

  val Cursor = "cursor"
  val Changes = false
  val Backoff = 60

  val Metadata = LongpollMetadata(Changes, Some(Backoff))

  val LongpollMetadataJson = s"""
  {
      "changes": $Changes,
      "backoff": $Backoff
  }
  """

  val BadRequestFailure = """{"error" : "Bad URL query string"}"""

  describe("Longpoll Delta") {

    it("should make an http request") {
      val probe = ioProbe

      dropbox longpoll_delta (probe ref, Cursor)

      val expectedURI = s"https://api-notify.dropbox.com/1/longpoll_delta?cursor=$Cursor"
      probe expectMsg HttpRequest(uri = expectedURI, headers = List(authorizationHeader, userAgentHeader))
    }

    it("should poll for a specific time") {
      val probe = ioProbe
      val timeout = 90

      val response = dropbox longpoll_delta (probe ref, Cursor, Some(timeout))

      val request = probe expectMsgClass classOf[HttpRequest]

      request.uri.query.get("timeout").get should be(timeout.toString)
    }

    it("should return longpoll metadata") {
      val probe = ioProbe

      val response = dropbox longpoll_delta (probe ref, Cursor)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(OK, HttpEntity(`text/plain`, LongpollMetadataJson)))

      await(response) should be(Metadata)
    }

    it("should propagate bad request failures") {
      val probe = ioProbe

      val response = dropbox longpoll_delta (probe ref, "")

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(BadRequest, HttpEntity(`text/plain`, BadRequestFailure)))

      val error = intercept[UnsuccessfulResponseException] { await(response) }
      error.getMessage should be(s"Status: $BadRequest\nBody: $BadRequestFailure")
    }
  }
}