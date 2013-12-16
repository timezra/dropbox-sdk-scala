package timezra.dropbox.core

import spray.json.DefaultJsonProtocol
import akka.actor.ActorRef
import akka.util.Timeout
import spray.http.HttpData
import java.util.Date
import spray.json.JsString
import spray.json.JsValue
import spray.json.DeserializationException
import java.text.DateFormat
import java.text.SimpleDateFormat
import spray.json.RootJsonFormat
import spray.http.HttpHeader
import spray.http.HttpRequest

case class QuotaInfo(datastores: Int, shared: Long, quota: Long, normal: Long)
case class AccountInfo(referral_link: String, display_name: String, uid: Long, country: Option[String], quota_info: QuotaInfo, email: String)
object AccountInfoJsonProtocol extends DefaultJsonProtocol {
  implicit val quotaInfoFormat = jsonFormat4(QuotaInfo)
  implicit def accountInfoFormat = jsonFormat6(AccountInfo)
}

case class ContentMetadata(size: String, bytes: Long, path: String, is_dir: Boolean, is_deleted: Option[Boolean], rev: Option[String], thumb_exists: Boolean, icon: String, modified: Option[Date], client_mtime: Date, root: String, mime_type: String, revision: Option[Long])
object ContentMetadataJsonProtocol extends DefaultJsonProtocol {
  implicit object DateJsonFormat extends RootJsonFormat[Date] {
    def write(date: Date) = {
      JsString(formatter format date)
    }
    def read(value: JsValue) = value match {
      case null ⇒ null
      case JsString(date) ⇒ formatter parse date
      case _ ⇒ throw new DeserializationException("Date Expected with format %a, %d %b %Y %H:%M:%S %z")
    }
    def formatter: DateFormat = new SimpleDateFormat("EEE, d MMM yyyy HH:mm:ss Z")
  }
  implicit def contentMetadataFormat = jsonFormat13(ContentMetadata)
}

case class ByteRange(start: Option[Long], end: Option[Long]) {
  require(start.isDefined || end.isDefined)

  override def toString = s"""${start.getOrElse("")}-${end.getOrElse("")}"""
}

object ContentTypes {
  import spray.http.MediaType
  import spray.http.MediaTypes
  val `text/javascript` = MediaType custom ("text", "javascript", true, true)
  MediaTypes register `text/javascript`
}

object Dropbox {
  def apply(clientIdentifier: String, accessToken: String): Dropbox = new Dropbox(clientIdentifier, accessToken)
}

class Dropbox(clientIdentifier: String, accessToken: String) {

  import scala.util.{ Failure, Success }
  import akka.actor.ActorSystem
  import akka.io.IO
  import spray.can.Http
  import scala.concurrent.Future
  import scala.concurrent.duration.DurationInt
  import spray.client.pipelining._
  import spray.httpx.unmarshalling.Unmarshaller
  import spray.httpx.unmarshalling.FromResponseUnmarshaller
  import spray.http.HttpEntity
  import spray.http.HttpResponse

  implicit lazy val system = ActorSystem("dropbox-sdk-scala")
  import system.dispatcher

  def accountInfo(conduit: ActorRef = IO(Http))(implicit timeout: Timeout = 60 seconds): Future[AccountInfo] = {
    import AccountInfoJsonProtocol.accountInfoFormat
    import spray.json.jsonReader
    import spray.json.JsonParser
    import spray.json.RootJsonReader
    import spray.http.HttpCharsets
    import HttpEntity.NonEmpty

    implicit def sprayJsonUnmarshaller[T: RootJsonReader] = Unmarshaller[T](ContentTypes.`text/javascript`) {
      case x: NonEmpty ⇒
        val json = JsonParser(x.asString(defaultCharset = HttpCharsets.`UTF-8`))
        jsonReader[T] read json
    }
    val pipeline = (
      addHeader("User-Agent", s"${clientIdentifier} Dropbox-Scala-SDK/1.0") ~>
      addHeader("Authorization", s"Bearer ${accessToken}") ~>
      sendReceive(conduit) ~>
      unmarshal[AccountInfo]
    )
    pipeline {
      Get("https://api.dropbox.com/1/account/info")
    }
  }

  def getFile(conduit: ActorRef = IO(Http), root: String = "auto", path: String, rev: Option[String] = None, range: Option[Seq[ByteRange]] = None)(implicit timeout: Timeout = 15 minutes, maxChunkSize: Long = 1048576): Future[Tuple2[ContentMetadata, Stream[HttpData]]] = {
    implicit val FileUnmarshaller = new FromResponseUnmarshaller[Tuple2[ContentMetadata, Stream[HttpData]]] {
      import spray.json._
      import DefaultJsonProtocol._
      import ContentMetadataJsonProtocol.contentMetadataFormat

      def apply(response: HttpResponse) = {
        val metadataHeader = response.headers.find(_.name == "x-dropbox-metadata")
        Right(Tuple2(metadataHeader.get.value.asJson.convertTo, response.entity.data.toChunkStream(maxChunkSize)))
      }
    }
    val pipeline = (
      addHeader("User-Agent", s"${clientIdentifier} Dropbox-Scala-SDK/1.0") ~>
      addHeader("Authorization", s"Bearer ${accessToken}") ~>
      range.fold(identity[HttpRequest]_)(r ⇒ addHeader("Range", s"""bytes=${r.mkString("", ",", "")}""")) ~>
      sendReceive(conduit) ~>
      unmarshal[Tuple2[ContentMetadata, Stream[HttpData]]]
    )
    val query = rev.fold("")(v ⇒ s"?rev=$v")
    pipeline {
      Get(s"https://api-content.dropbox.com/1/files/$root/$path$query")
    }
  }

  def shutdown(): Unit = {
    import akka.pattern.ask
    import spray.util.pimpFuture

    IO(Http).ask(Http.CloseAll)(3 second).await
    system.shutdown()
  }
}
