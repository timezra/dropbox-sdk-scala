package timezra.dropbox.core

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import org.junit.runner.RunWith
import org.scalatest.BeforeAndAfterAll
import org.scalatest.FunSpecLike
import org.scalatest.junit.JUnitRunner
import org.scalatest.Matchers
import akka.actor.ActorSystem
import akka.io.IO
import akka.testkit.TestKit
import akka.testkit.TestProbe
import spray.can.Http
import spray.http.ContentType.apply
import spray.http.HttpEntity
import spray.http.HttpHeaders.RawHeader
import spray.http.HttpRequest
import spray.http.HttpResponse
import spray.http.parser.HttpParser
import spray.http.StatusCodes
import spray.httpx.UnsuccessfulResponseException
import spray.http.HttpHeader

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
      val expectedHeaders = List(header("Authorization", s"Bearer $AccessToken"), header("User-Agent", s"$ClientIdentifier Dropbox-Scala-SDK/1.0"))
      probe expectMsg HttpRequest(uri = expectedURI, headers = expectedHeaders)
    }

    it("should parse account info") {
      val probe = ioProbe

      val response = dropbox accountInfo probe.ref

      probe expectMsgClass classOf[HttpRequest]
      probe.reply(HttpResponse(entity = HttpEntity(ContentTypes.`text/javascript`, SuccessfulResponse)))

      val actual = await(response)
      actual shouldEqual Info
    }

    it("should propagate authorization failures") {
      val probe = ioProbe

      val response = dropbox accountInfo probe.ref

      probe expectMsgClass classOf[HttpRequest]
      probe.reply(HttpResponse(status = StatusCodes.Unauthorized, entity = HttpEntity(ContentTypes.`text/javascript`, AuthorizationFailure)))

      intercept[UnsuccessfulResponseException] { await(response) }
    }
  }
}