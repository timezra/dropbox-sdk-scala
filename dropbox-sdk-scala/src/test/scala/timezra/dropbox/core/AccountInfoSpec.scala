package timezra.dropbox.core

import org.junit.runner.RunWith
import org.scalatest.BeforeAndAfterAll
import org.scalatest.FunSpecLike
import org.scalatest.junit.JUnitRunner
import akka.actor.ActorSystem
import akka.io.IO
import akka.testkit.TestKit
import akka.testkit.TestProbe
import spray.can.Http
import spray.http.HttpHeaders.RawHeader
import spray.http.HttpRequest
import spray.http.parser.HttpParser

@RunWith(classOf[JUnitRunner])
class AccountInfoSpec(_system: ActorSystem) extends TestKit(_system) with FunSpecLike with BeforeAndAfterAll {

  def this() = this(ActorSystem("AccountInfoSpec"))

  override def afterAll {
    TestKit shutdownActorSystem system
  }

  describe("Account Info") {
    it("should make an http request") {
      val clientIdentifier = "fake_client_identifier"
      val accessToken = "fake_access_token"
      val dropbox = Dropbox(clientIdentifier, accessToken)

      val probe = TestProbe()
      probe watch IO(Http)

      dropbox accountInfo probe.ref

      probe expectMsg HttpRequest(uri = "https://api.dropbox.com/1/account/info", headers = List(header("Authorization", s"Bearer $accessToken"), header("User-Agent", s"$clientIdentifier Dropbox-Scala-SDK/1.0")))
    }
  }

  private def header(n: String, v: String) = HttpParser.parseHeader(RawHeader(n, v)).right.get
}