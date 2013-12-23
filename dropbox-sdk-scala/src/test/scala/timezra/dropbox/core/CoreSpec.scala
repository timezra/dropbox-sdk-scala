package timezra.dropbox.core

import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

import org.scalatest.BeforeAndAfterAll
import org.scalatest.FunSpecLike
import org.scalatest.Matchers

import akka.actor.ActorSystem
import akka.io.IO
import akka.testkit.TestKit
import akka.testkit.TestProbe
import spray.can.Http
import spray.http.HttpHeader
import spray.http.HttpHeaders.RawHeader
import spray.http.parser.HttpParser

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
  protected def header[V <% String](n: String, v: V): HttpHeader = HttpParser.parseHeader(RawHeader(n, v)).right.get
  protected val authorizationHeader = header("Authorization", s"Bearer $AccessToken")
  protected val userAgentHeader = header("User-Agent", s"$ClientIdentifier Dropbox-Scala-SDK/1.0")
  protected def await[T](h: Future[T]): T = Await result (h, 1 second)
}