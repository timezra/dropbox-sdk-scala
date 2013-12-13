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

@RunWith(classOf[JUnitRunner])
class AccountInfoSpec(_system: ActorSystem) extends TestKit(_system) with FunSpecLike with Matchers with BeforeAndAfterAll {

  def this() = this(ActorSystem("AccountInfoSpec"))

  override def afterAll {
    TestKit shutdownActorSystem system
  }

  describe("Account Info") {

    val ClientIdentifier = "fake_client_identifier"
    val AccessToken = "fake_access_token"

    it("should make an http request") {
      val probe = TestProbe()
      probe watch IO(Http)

      Dropbox(ClientIdentifier, AccessToken) accountInfo probe.ref

      probe expectMsg HttpRequest(uri = "https://api.dropbox.com/1/account/info", headers = List(header("Authorization", s"Bearer $AccessToken"), header("User-Agent", s"$ClientIdentifier Dropbox-Scala-SDK/1.0")))
    }

    it("should parse account info") {
      val probe = TestProbe()
      probe watch IO(Http)

      val response = Dropbox(ClientIdentifier, AccessToken) accountInfo probe.ref

      probe expectMsgClass classOf[HttpRequest]
      probe.reply(HttpResponse(entity = HttpEntity(ContentTypes.`text/javascript`, SuccessfulResponse)))

      val actual = Await result (response, 1 second)
      actual shouldEqual Info
    }

    it("should propagate failures") {
      val probe = TestProbe()
      probe watch IO(Http)

      val response = Dropbox(ClientIdentifier, AccessToken) accountInfo probe.ref

      probe expectMsgClass classOf[HttpRequest]
      probe.reply(HttpResponse(status = StatusCodes.Unauthorized, entity = HttpEntity(ContentTypes.`text/javascript`, UnsuccessfulResponse)))

      intercept[UnsuccessfulResponseException] { Await result (response, 1 second) }
    }
  }

  private def header(n: String, v: String) = HttpParser.parseHeader(RawHeader(n, v)).right.get

  val Info = AccountInfo("https://db.tt/referralLink", "Display Name", 12345678, "KG", QuotaInfo(0, 1, 2, 3), "test@email.com")

  val SuccessfulResponse = s"""
    {"referral_link": "${Info.referral_link}", 
     "display_name": "${Info.display_name}", 
     "uid": ${Info.uid}, 
     "country": "${Info.country}", 
     "quota_info": {"datastores": ${Info.quota_info.datastores}, 
                    "shared": ${Info.quota_info.shared}, 
                    "quota": ${Info.quota_info.quota}, 
                    "normal": ${Info.quota_info.normal}}, 
                    "email": "${Info.email}"}
  """
  val UnsuccessfulResponse = s"""
    {"error": "The given OAuth 2 access token doesn't exist or has expired."}
  """
}