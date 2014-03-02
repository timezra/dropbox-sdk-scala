package timezra.dropbox.core

import java.text.DateFormat
import java.text.SimpleDateFormat
import java.util.Date
import scala.annotation.implicitNotFound
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.io.IO
import akka.pattern.ask
import akka.util.Timeout
import akka.util.Timeout.durationToTimeout
import spray.can.Http
import spray.client.pipelining.Get
import spray.client.pipelining.Put
import spray.client.pipelining.WithTransformerConcatenation
import spray.client.pipelining.addHeader
import spray.client.pipelining.sendReceive
import spray.client.pipelining.unmarshal
import spray.http.ContentTypeRange.apply
import spray.http.HttpCharsets
import spray.http.HttpData
import spray.http.HttpEntity.NonEmpty
import spray.http.HttpRequest
import spray.http.HttpResponse
import spray.http.MediaType
import spray.http.MediaTypes
import spray.httpx.unmarshalling.FromResponseUnmarshaller
import spray.httpx.unmarshalling.Unmarshaller
import spray.json.DefaultJsonProtocol
import spray.json.DeserializationException
import spray.json.JsString
import spray.json.JsValue
import spray.json.JsonParser
import spray.json.RootJsonFormat
import spray.json.RootJsonReader
import spray.json.jsonReader
import spray.json.pimpString
import spray.util.pimpFuture
import spray.http.Uri
import spray.http.Uri.Query
import java.util.Locale

case class QuotaInfo(datastores: Int, shared: Long, quota: Long, normal: Long)
case class AccountInfo(referral_link: String, display_name: String, uid: Long, country: Option[String], quota_info: QuotaInfo, email: String)
object AccountInfoJsonProtocol extends DefaultJsonProtocol {
  implicit val quotaInfoFormat = jsonFormat4(QuotaInfo)
  implicit def accountInfoFormat = jsonFormat6(AccountInfo)
}

case class ContentMetadata(size: String, bytes: Long, path: String, is_dir: Boolean, is_deleted: Option[Boolean], rev: Option[String], hash: Option[String], thumb_exists: Boolean, icon: String, modified: Option[Date], client_mtime: Option[Date], root: String, mime_type: Option[String], revision: Option[Long], contents: Option[List[ContentMetadata]])
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
  implicit def contentMetadataFormat: RootJsonFormat[ContentMetadata] = rootFormat(lazyFormat(jsonFormat15(ContentMetadata)))
}

case class DeltaMetadata(entries: List[Tuple2[String, ContentMetadata]], reset: Boolean, cursor: String, has_more: Boolean)
object DeltaMetadataJsonProtocol extends DefaultJsonProtocol {
  import ContentMetadataJsonProtocol.contentMetadataFormat
  implicit def deltaMetadataFormat = jsonFormat4(DeltaMetadata)
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

  def addUserAgent = addHeader("User-Agent", s"${clientIdentifier} Dropbox-Scala-SDK/1.0")
  def addAuthorization = addHeader("Authorization", s"Bearer ${accessToken}")

  def accountInfo(conduit: ActorRef = IO(Http))(implicit timeout: Timeout = 60 seconds, locale: Option[Locale] = None): Future[AccountInfo] = {
    import AccountInfoJsonProtocol.accountInfoFormat
    import SprayJsonSupport._

    val pipeline = (
      addUserAgent ~>
      addAuthorization ~>
      sendReceive(conduit) ~>
      unmarshal[AccountInfo]
    )
    val q = Seq(locale map ("locale" -> _.toLanguageTag)) flatMap (f ⇒ f)
    pipeline {
      Get(Uri("https://api.dropbox.com/1/account/info") withQuery (q: _*))
    }
  }

  def getFile(conduit: ActorRef = IO(Http),
    root: String = "auto",
    path: String, rev: Option[String] = None,
    range: Option[Seq[ByteRange]] = None)(implicit timeout: Timeout = 15 minutes, maxChunkSize: Long = 1048576): Future[Tuple2[ContentMetadata, Stream[HttpData]]] = {
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
      addUserAgent ~>
      addAuthorization ~>
      range.fold(identity[HttpRequest]_)(r ⇒ addHeader("Range", s"""bytes=${r mkString ("", ",", "")}""")) ~>
      sendReceive(conduit) ~>
      unmarshal[(ContentMetadata, Stream[HttpData])]
    )
    val q = Seq(rev map ("rev" ->)) flatMap (f ⇒ f)
    pipeline {
      Get(Uri(s"https://api-content.dropbox.com/1/files/$root/$path") withQuery (q: _*))
    }
  }

  import scalaz.effect.IoExceptionOr
  import scalaz.iteratee.EnumeratorT
  import scalaz.effect.{ IO ⇒ zIO }
  def putFile(conduit: ActorRef = IO(Http),
    root: String = "auto",
    path: String,
    contents: EnumeratorT[IoExceptionOr[(Array[Byte], Int)], zIO],
    length: Int,
    rev: Option[String] = None,
    parent_rev: Option[String] = None,
    overwrite: Option[Boolean] = None)(implicit timeout: Timeout = 15 minutes, locale: Option[Locale] = None): Future[ContentMetadata] = {
    import ContentMetadataJsonProtocol.contentMetadataFormat
    import SprayJsonSupport._
    import MetaMarshallers._
    import Arrays._

    implicit def boundArray2HttpData(t: (Array[Byte], Int)): HttpData = HttpData(t._1 takeT t._2)

    val pipeline = (
      addUserAgent ~>
      addAuthorization ~>
      addHeader("Content-Length", String valueOf (length)) ~>
      sendReceive(conduit) ~>
      unmarshal[ContentMetadata]
    )
    val q = Seq(parent_rev map ("parent_rev" ->), overwrite map ("overwrite" -> _.toString), locale map ("locale" -> _.toLanguageTag)) flatMap (f ⇒ f)
    pipeline {
      Put(Uri(s"https://api-content.dropbox.com/1/files_put/$root/$path") withQuery (q: _*), contents)
    }
  }

  import java.io.File
  import spray.http.BodyPart
  def postFile(conduit: ActorRef = IO(Http),
    root: String = "auto",
    path: String,
    file: File,
    filename: Option[String] = None,
    parent_rev: Option[String] = None,
    overwrite: Option[Boolean] = None)(implicit timeout: Timeout = 15 minutes, locale: Option[Locale] = None): Future[ContentMetadata] = {
    import ContentMetadataJsonProtocol.contentMetadataFormat
    import SprayJsonSupport._
    import spray.http.MultipartFormData
    import spray.http.HttpHeaders.`Content-Disposition`
    import spray.httpx.marshalling.MultipartMarshallers._

    val pipeline = (
      addUserAgent ~>
      addAuthorization ~>
      sendReceive(conduit) ~>
      unmarshal[ContentMetadata]
    )
    val payload = MultipartFormData(Map(
      "dropbox-file" -> BodyPart(
        HttpEntity(HttpData(file)),
        `Content-Disposition`("form-data", Map("name" -> "file", "filename" -> filename.getOrElse(file getName))) :: Nil
      )
    ))

    val q = Seq(parent_rev map ("parent_rev" ->), overwrite map ("overwrite" -> _.toString), locale map ("locale" -> _.toLanguageTag)) flatMap (f ⇒ f)

    pipeline {
      Post(Uri(s"https://api-content.dropbox.com/1/files/$root/$path") withQuery (q: _*), payload)
    }
  }

  def metadata(conduit: ActorRef = IO(Http),
    root: String = "auto",
    path: String,
    file_limit: Option[Int] = None,
    hash: Option[String] = None,
    list: Option[Boolean] = None,
    include_deleted: Option[Boolean] = None,
    rev: Option[String] = None)(implicit timeout: Timeout = 60 seconds, locale: Option[Locale] = None): Future[Either[Boolean, ContentMetadata]] = {

    import spray.http.StatusCodes
    import spray.json._
    import DefaultJsonProtocol._
    import ContentMetadataJsonProtocol.contentMetadataFormat

    implicit val NotModifiedOrResultUnmarshaller = new FromResponseUnmarshaller[Either[Boolean, ContentMetadata]] {
      def apply(response: HttpResponse) = response.status match {
        case StatusCodes.NotModified ⇒ Right(Left(true))
        case StatusCodes.Success(_) ⇒ Right(Right(response.entity.asString.asJson.convertTo))
        case _ ⇒ throw new spray.httpx.UnsuccessfulResponseException(response)
      }
    }

    val pipeline = (
      addUserAgent ~>
      addAuthorization ~>
      sendReceive(conduit) ~>
      unmarshal[Either[Boolean, ContentMetadata]]
    )
    val q = Seq(locale map ("locale" -> _.toLanguageTag),
      file_limit map ("file_limit" -> _.toString),
      hash map ("hash" ->),
      list map ("list" -> _.toString),
      include_deleted map ("include_deleted" -> _.toString),
      rev map ("rev" ->)) flatMap (f ⇒ f)
    pipeline {
      Get(Uri(s"https://api.dropbox.com/1/metadata/$root/$path") withQuery (q: _*))
    }
  }

  import spray.http.BodyPart
  def delta(conduit: ActorRef = IO(Http),
    path_prefix: Option[String] = None,
    cursor: Option[String] = None)(implicit timeout: Timeout = 60 seconds, locale: Option[Locale] = None): Future[DeltaMetadata] = {
    import DeltaMetadataJsonProtocol.deltaMetadataFormat
    import SprayJsonSupport._
    import spray.httpx.marshalling.MultipartMarshallers._
    import spray.http.FormData

    val pipeline = (
      addUserAgent ~>
      addAuthorization ~>
      sendReceive(conduit) ~>
      unmarshal[DeltaMetadata]
    )
    val payload = Seq(
      path_prefix map ("path_prefix" ->),
      cursor map ("cursor" ->),
      locale map ("locale" -> _.toLanguageTag)
    ) flatMap (f ⇒ f)

    pipeline {
      Post(Uri(s"https://api.dropbox.com/1/delta"), FormData(payload))
    }
  }

  def shutdown(): Unit = {
    import akka.pattern.ask
    import spray.util.pimpFuture

    IO(Http).ask(Http.CloseAll)(3 seconds).await
    system shutdown
  }
}
