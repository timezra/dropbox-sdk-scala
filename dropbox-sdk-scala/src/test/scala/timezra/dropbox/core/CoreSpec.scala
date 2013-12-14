package timezra.dropbox.core

import akka.actor.ActorSystem
import akka.testkit.TestKit
import org.scalatest.FunSpecLike
import org.scalatest.Matchers
import org.scalatest.BeforeAndAfterAll
import spray.http.HttpHeader
import spray.http.parser.HttpParser
import akka.testkit.TestProbe
import akka.io.IO
import spray.can.Http
import spray.http.HttpHeaders.RawHeader

abstract class CoreSpec(_system: ActorSystem) extends TestKit(_system) with FunSpecLike with Matchers with BeforeAndAfterAll {

  def this() = this(ActorSystem("dropbox-sdk-scala-test"))

  val ClientIdentifier = "fake_client_identifier"
  val AccessToken = "fake_access_token"

  override def afterAll {
    TestKit shutdownActorSystem system
  }

  protected def header(n: String, v: String): HttpHeader = HttpParser.parseHeader(RawHeader(n, v)).right.get
  protected def conduitProbe: TestProbe = {
    val probe = TestProbe()
    probe watch IO(Http)
    probe
  }
}