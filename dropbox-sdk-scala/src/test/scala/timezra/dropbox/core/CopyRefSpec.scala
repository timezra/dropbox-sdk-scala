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
class CopyRefSpec extends CoreSpec with Inside {

  val Root = "root"
  val Path = "test.txt"

  val CopyRefWithExpiry = ReferenceWithExpiry("copy_ref", formatter.parse("Tue, 01 Jan 2030 00:00:00 +0000"))
  def formatter: DateFormat = new SimpleDateFormat("EEE, d MMM yyyy HH:mm:ss Z")

  val ReferenceWithExpiryJson = s"""
  {
      "copy_ref": "${CopyRefWithExpiry.copy_ref}",
      "expires": "${formatter.format(CopyRefWithExpiry.expires)}"
  }
  """

  val NotFoundFailure = s"""{"error": "The file or folder does't exist. Can't create copy ref."}"""

  describe("CopyRef") {

    it("should make an http request") {
      val probe = ioProbe

      dropbox copy_ref (probe ref, Root, Path)

      val expectedURI = s"https://api.dropbox.com/1/copy_ref/$Root/$Path"
      probe expectMsg HttpRequest(uri = expectedURI, headers = List(authorizationHeader, userAgentHeader))
    }

    it("should return a reference with an expiry") {
      val probe = ioProbe

      val response = dropbox copy_ref (probe ref, path = Path)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(OK, HttpEntity(`text/javascript`, ReferenceWithExpiryJson)))

      val referenceWithExpiry = await(response)

      referenceWithExpiry should be(CopyRefWithExpiry)
    }

    it("should propagate not found failures") {
      val probe = ioProbe

      val response = dropbox copy_ref (probe ref, path = Path)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(NotFound, HttpEntity(`application/json`, NotFoundFailure)))

      val error = intercept[UnsuccessfulResponseException] { await(response) }
      error.getMessage should be(s"Status: $NotFound\nBody: $NotFoundFailure")
    }
  }
}