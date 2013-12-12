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

      val expected = AccountInfo(ReferralLink, DisplayName, Uid, Country, QuotaInfo(Datastores, Shared, Quota, Normal), Email)
      val actual = Await result (response, 1 second)
      actual shouldEqual expected
    }
  }

  private def header(n: String, v: String) = HttpParser.parseHeader(RawHeader(n, v)).right.get

  val ReferralLink = "https://db.tt/alink"
  val DisplayName = "Test Name"
  val Uid = 12345678
  val Country = "KG"
  val Datastores = 0
  val Shared = 1
  val Quota = 2
  val Normal = 3
  val Email = "test@name.com"

  val SuccessfulResponse = s"""
    {"referral_link": "$ReferralLink", 
     "display_name": "$DisplayName", 
     "uid": $Uid, 
     "country": "$Country", 
     "quota_info": {"datastores": $Datastores, 
                    "shared": $Shared, 
                    "quota": $Quota, 
                    "normal": $Normal}, 
                    "email": "$Email"}
  """
}