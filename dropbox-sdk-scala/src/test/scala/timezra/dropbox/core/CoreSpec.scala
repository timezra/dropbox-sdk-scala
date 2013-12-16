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
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.concurrent.duration.DurationInt
import scala.concurrent.Await
import scala.concurrent.Future

abstract class CoreSpec(_system: ActorSystem) extends TestKit(_system) with FunSpecLike with Matchers with BeforeAndAfterAll {

  def this() = this(ActorSystem("dropbox-sdk-scala-test"))

  val ClientIdentifier = "client_identifier"
  val AccessToken = "access_token"

  val AuthorizationFailure = s"""
    {"error": "The given OAuth 2 access token doesn't exist or has expired."}
  """

  override def afterAll {
    TestKit shutdownActorSystem system
  }

  protected def dropbox: Dropbox = Dropbox(ClientIdentifier, AccessToken)
  protected def ioProbe: TestProbe = {
    val probe = TestProbe()
    probe watch IO(Http)
    probe
  }
  protected def header(n: String, v: String): HttpHeader = HttpParser.parseHeader(RawHeader(n, v)).right.get
  protected def await[T](h: Future[T]): T = Await result (h, 1 second)
}