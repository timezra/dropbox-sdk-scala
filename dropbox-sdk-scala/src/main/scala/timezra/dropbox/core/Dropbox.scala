package timezra.dropbox.core

import spray.json.DefaultJsonProtocol
import akka.actor.ActorRef
import akka.util.Timeout

case class QuotaInfo(datastores: Int, shared: Long, quota: Long, normal: Long)
case class AccountInfo(referral_link: String, display_name: String, uid: Long, country: String, quota_info: QuotaInfo, email: String)
object AccountInfoJsonProtocol extends DefaultJsonProtocol {
  implicit val quotaInfoFormat = jsonFormat4(QuotaInfo)
  implicit def accountInfoFormat = jsonFormat6(AccountInfo)
}

object Dropbox {
  def apply(clientIdentifier: String, accessToken: String): Dropbox = new Dropbox(clientIdentifier, accessToken)
}

class Dropbox(clientIdentifier: String, accessToken: String) {

  import scala.util.{ Failure, Success }
  import akka.actor.ActorSystem
  import akka.io.IO
  import spray.can.Http
  import spray.http.MediaType
  import spray.http.MediaTypes
  import scala.concurrent.Future
  import scala.concurrent.duration.DurationInt

  implicit val system = ActorSystem("dropbox-sdk-scala")
  import system.dispatcher

  val `text/javascript` = MediaType.custom("text", "javascript", true, true)
  MediaTypes.register(`text/javascript`)

  def accountInfo(actorRef: ActorRef = IO(Http))(implicit timeout: Timeout = 60.seconds): Future[AccountInfo] = {
    import spray.client.pipelining._
    import AccountInfoJsonProtocol.accountInfoFormat
    import spray.json.jsonReader
    import spray.json.JsonParser
    import spray.json.RootJsonReader
    import spray.httpx.unmarshalling.Unmarshaller
    import spray.http.HttpEntity.NonEmpty
    import spray.http.HttpCharsets
    implicit def sprayJsonUnmarshaller[T: RootJsonReader] = Unmarshaller[T](`text/javascript`) {
      case x: NonEmpty ⇒
        val json = JsonParser(x.asString(defaultCharset = HttpCharsets.`UTF-8`))
        jsonReader[T].read(json)
    }
    val pipeline = (
      addHeader("User-Agent", s"${clientIdentifier} Dropbox-Scala-SDK/1.0") ~>
      addHeader("Authorization", s"Bearer ${accessToken}") ~>
      sendReceive(actorRef) ~> unmarshal[AccountInfo]
    )
    pipeline {
      Get("https://api.dropbox.com/1/account/info")
    }
  }

  def shutdown(): Unit = {
    import akka.pattern.ask
    import spray.util.pimpFuture

    IO(Http).ask(Http.CloseAll)(3.second).await
    system.shutdown()
  }
}
