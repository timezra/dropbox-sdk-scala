package timezra.dropbox.core

import org.junit.runner.RunWith
import spray.http.HttpEntity
import spray.http.HttpRequest
import spray.http.HttpResponse
import spray.http.StatusCodes
import spray.httpx.UnsuccessfulResponseException
import org.scalatest.junit.JUnitRunner
import ContentTypes.`text/javascript`
import java.util.Locale

@RunWith(classOf[JUnitRunner])
class AccountInfoSpec extends CoreSpec {

  val Info = AccountInfo("https://db.tt/referralLink", "Display Name", 12345678, Some("KG"), QuotaInfo(0, 1, 2, 3), "test@email.com")
  val SuccessfulResponse = s"""
  {
      "referral_link": "${Info.referral_link}", 
      "display_name": "${Info.display_name}", 
      "uid": ${Info.uid}, 
      "country": "${Info.country.get}", 
      "quota_info": 
          {
              "datastores": ${Info.quota_info.datastores}, 
              "shared": ${Info.quota_info.shared}, 
              "quota": ${Info.quota_info.quota}, 
              "normal": ${Info.quota_info.normal}
          }, 
      "email": "${Info.email}"
  }
  """

  describe("Account Info") {

    it("should make an http request") {
      val probe = ioProbe

      dropbox accountInfo probe.ref

      val expectedURI = "https://api.dropbox.com/1/account/info"
      probe expectMsg HttpRequest(uri = expectedURI, headers = List(authorizationHeader, userAgentHeader))
    }

    it("should parse account info") {
      val probe = ioProbe

      val response = dropbox accountInfo probe.ref

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(entity = HttpEntity(`text/javascript`, SuccessfulResponse)))

      await(response) should be(Info)
    }

    it("should request language specific text") {
      val probe = ioProbe

      implicit val locale = Some(Locale.CHINA)
      val response = dropbox accountInfo probe.ref

      val request = probe expectMsgClass classOf[HttpRequest]

      request.uri.query.get("locale") should be(locale.map(_.toLanguageTag))
    }

    it("should propagate authorization failures") {
      val probe = ioProbe

      val response = dropbox accountInfo probe.ref

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(status = StatusCodes.Unauthorized, entity = HttpEntity(`text/javascript`, AuthorizationFailure)))

      intercept[UnsuccessfulResponseException] { await(response) }
    }
  }
}