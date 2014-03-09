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
import spray.json.JsArray
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

object JsonImplicits {
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

  implicit object UriJsonFormat extends RootJsonFormat[Uri] {
    def write(uri: Uri) = JsString(uri.toString)
    def read(value: JsValue) = value match {
      case null ⇒ null
      case JsString(uri) ⇒ Uri(uri)
      case _ ⇒ throw new DeserializationException("Expected URI")
    }
  }
}

case class ContentMetadata(size: String, bytes: Long, path: String, is_dir: Boolean, is_deleted: Option[Boolean], rev: Option[String], hash: Option[String], thumb_exists: Boolean, icon: String, modified: Option[Date], client_mtime: Option[Date], root: String, mime_type: Option[String], revision: Option[Long], contents: Option[List[ContentMetadata]])
object ContentMetadataJsonProtocol extends DefaultJsonProtocol {
  import JsonImplicits._
  implicit def contentMetadataFormat: RootJsonFormat[ContentMetadata] = rootFormat(lazyFormat(jsonFormat15(ContentMetadata)))
}

case class DeltaMetadata(entries: List[Tuple2[String, ContentMetadata]], reset: Boolean, cursor: String, has_more: Boolean)
object DeltaMetadataJsonProtocol extends DefaultJsonProtocol {
  import ContentMetadataJsonProtocol.contentMetadataFormat
  implicit def deltaMetadataFormat = jsonFormat4(DeltaMetadata)
}

case class LongpollMetadata(changes: Boolean, backoff: Option[Int])
object LongpollMetadataJsonProtocol extends DefaultJsonProtocol {
  implicit def longpollMetadataFormat = jsonFormat2(LongpollMetadata)
}

case class LinkWithExpiry(url: Uri, expires: Date)
object LinkWithExpiryJsonProtocol extends DefaultJsonProtocol {
  import spray.http.Uri
  import JsonImplicits._
  implicit def linkWithExpiryFormat: RootJsonFormat[LinkWithExpiry] = jsonFormat2(LinkWithExpiry)
}

case class ReferenceWithExpiry(copy_ref: String, expires: Date)
object ReferenceWithExpiryJsonProtocol extends DefaultJsonProtocol {
  import JsonImplicits._
  implicit def referenceWithExpiryFormat: RootJsonFormat[ReferenceWithExpiry] = jsonFormat2(ReferenceWithExpiry)
}

case class UploadWithExpiry(upload_id: String, offset: Long, expires: Date)
object UploadWithExpiryJsonProtocol extends DefaultJsonProtocol {
  import JsonImplicits._
  implicit def uploadWithExpiryFormat: RootJsonFormat[UploadWithExpiry] = jsonFormat3(UploadWithExpiry)
}

case class ByteRange(start: Option[Long], end: Option[Long]) {
  require(start.isDefined || end.isDefined)

  override def toString = s"""${start.getOrElse("")}-${end.getOrElse("")}"""
}

object ContentTypes {
  import spray.http.MediaType
  import spray.http.MediaTypes
  val `text/javascript` = MediaType custom ("text", "javascript", true, true)
  val `image/png` = MediaType custom ("image", "png", true, true)
  val `image/jpeg` = MediaType custom ("image", "jpeg", true, true)
  MediaTypes register `text/javascript`
  MediaTypes register `image/png`
  MediaTypes register `image/jpeg`
}

object Format extends Enumeration {
  type Format = Value
  val jpeg = Value("jpeg")
  val png = Value("png")
}

object Size extends Enumeration {
  type Size = Value
  val xs = Value("xs")
  val s = Value("s")
  val m = Value("m")
  val l = Value("l")
  val xl = Value("xl")
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
    import SprayJsonSupport.sprayJsonUnmarshaller

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
    import SprayJsonSupport.sprayJsonUnmarshaller
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
    import SprayJsonSupport.sprayJsonUnmarshaller
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

  def delta(conduit: ActorRef = IO(Http),
    path_prefix: Option[String] = None,
    cursor: Option[String] = None)(implicit timeout: Timeout = 60 seconds, locale: Option[Locale] = None): Future[DeltaMetadata] = {
    import DeltaMetadataJsonProtocol.deltaMetadataFormat
    import SprayJsonSupport.sprayJsonUnmarshaller
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

  def longpoll_delta(conduit: ActorRef = IO(Http), cursor: String, timeout: Option[Int] = None)(implicit futureTimeout: Timeout = timeout getOrElse 30 seconds, locale: Option[Locale] = None): Future[LongpollMetadata] = {
    import LongpollMetadataJsonProtocol.longpollMetadataFormat
    import SprayJsonSupport.sprayPlainTextJsonUnmarshaller

    val pipeline = (
      addUserAgent ~>
      addAuthorization ~>
      sendReceive(conduit) ~>
      unmarshal[LongpollMetadata]
    )
    val q = Seq(Some("cursor", cursor), timeout map ("timeout" -> _.toString)) flatMap (f ⇒ f)
    pipeline {
      Get(Uri("https://api-notify.dropbox.com/1/longpoll_delta") withQuery (q: _*))
    }
  }

  def revisions(conduit: ActorRef = IO(Http),
    root: String = "auto",
    path: String,
    rev_limit: Option[Int] = None)(implicit timeout: Timeout = 60 seconds, locale: Option[Locale] = None): Future[List[ContentMetadata]] = {

    import spray.json.CollectionFormats
    import ContentMetadataJsonProtocol.contentMetadataFormat
    import SprayJsonSupport.sprayJsonUnmarshaller
    import DefaultJsonProtocol._

    val pipeline = (
      addUserAgent ~>
      addAuthorization ~>
      sendReceive(conduit) ~>
      unmarshal[List[ContentMetadata]]
    )
    val q = Seq(rev_limit map ("rev_limit" -> _.toString), locale map ("locale" -> _.toLanguageTag)) flatMap (f ⇒ f)
    pipeline {
      Get(Uri(s"https://api.dropbox.com/1/revisions/$root/$path") withQuery (q: _*))
    }
  }

  def restore(conduit: ActorRef = IO(Http),
    root: String = "auto",
    path: String,
    rev: String)(implicit timeout: Timeout = 60 seconds, locale: Option[Locale] = None): Future[ContentMetadata] = {

    import ContentMetadataJsonProtocol.contentMetadataFormat
    import SprayJsonSupport.sprayJsonUnmarshaller
    import spray.http.FormData

    val pipeline = (
      addUserAgent ~>
      addAuthorization ~>
      sendReceive(conduit) ~>
      unmarshal[ContentMetadata]
    )
    val payload = Seq(
      Some("rev", rev),
      locale map ("locale" -> _.toLanguageTag)
    ) flatMap (f ⇒ f)

    pipeline {
      Post(Uri(s"https://api.dropbox.com/1/restore/$root/$path"), FormData(payload))
    }
  }

  import spray.http.HttpMethod
  import spray.http.HttpMethods.GET
  def search(conduit: ActorRef = IO(Http),
    root: String = "auto",
    path: Option[String] = None,
    query: String,
    file_limit: Option[Int] = None,
    include_deleted: Option[Boolean] = None,
    method: HttpMethod = GET)(implicit timeout: Timeout = 60 seconds, locale: Option[Locale] = None): Future[List[ContentMetadata]] = {

    import spray.http.FormData
    import spray.http.HttpMethods.POST
    import spray.json.CollectionFormats
    import ContentMetadataJsonProtocol.contentMetadataFormat
    import SprayJsonSupport.sprayJsonUnmarshaller
    import DefaultJsonProtocol._

    val pipeline = (
      addUserAgent ~>
      addAuthorization ~>
      sendReceive(conduit) ~>
      unmarshal[List[ContentMetadata]]
    )
    val payload = Seq(Some("query", query), file_limit map ("file_limit" -> _.toString), include_deleted map ("include_deleted" -> _.toString), locale map ("locale" -> _.toLanguageTag)) flatMap (f ⇒ f)
    val searchUri = Uri(Seq(Some("https://api.dropbox.com/1/search"), Some(root), path).flatten.mkString("/"))
    pipeline {
      method match {
        case GET ⇒ Get(searchUri withQuery (payload: _*))
        case POST ⇒ Post(searchUri, FormData(payload))
      }
    }
  }

  def shares(conduit: ActorRef = IO(Http),
    root: String = "auto",
    path: String,
    short_url: Option[Boolean] = None)(implicit timeout: Timeout = 60 seconds, locale: Option[Locale] = None): Future[LinkWithExpiry] = {

    import LinkWithExpiryJsonProtocol.linkWithExpiryFormat
    import SprayJsonSupport.sprayJsonUnmarshaller
    import spray.http.FormData

    val pipeline = (
      addUserAgent ~>
      addAuthorization ~>
      sendReceive(conduit) ~>
      unmarshal[LinkWithExpiry]
    )
    val payload = Seq(
      short_url map ("short_url" -> _.toString),
      locale map ("locale" -> _.toLanguageTag)
    ) flatMap (f ⇒ f)

    pipeline {
      Post(Uri(s"https://api.dropbox.com/1/shares/$root/$path"), FormData(payload))
    }
  }

  def media(conduit: ActorRef = IO(Http),
    root: String = "auto",
    path: String)(implicit timeout: Timeout = 60 seconds, locale: Option[Locale] = None): Future[LinkWithExpiry] = {

    import LinkWithExpiryJsonProtocol.linkWithExpiryFormat
    import SprayJsonSupport.sprayJsonUnmarshaller
    import spray.http.FormData

    val pipeline = (
      addUserAgent ~>
      addAuthorization ~>
      sendReceive(conduit) ~>
      unmarshal[LinkWithExpiry]
    )
    val payload = Seq(locale map ("locale" -> _.toLanguageTag)) flatMap (f ⇒ f)

    pipeline {
      Post(Uri(s"https://api.dropbox.com/1/media/$root/$path"), FormData(payload))
    }
  }

  def copy_ref(conduit: ActorRef = IO(Http),
    root: String = "auto",
    path: String)(implicit timeout: Timeout = 60 seconds): Future[ReferenceWithExpiry] = {

    import ReferenceWithExpiryJsonProtocol.referenceWithExpiryFormat
    import SprayJsonSupport.sprayJsonUnmarshaller
    import DefaultJsonProtocol._

    val pipeline = (
      addUserAgent ~>
      addAuthorization ~>
      sendReceive(conduit) ~>
      unmarshal[ReferenceWithExpiry]
    )
    pipeline {
      Get(Uri(s"https://api.dropbox.com/1/copy_ref/$root/$path"))
    }
  }

  import Format._
  import Size._
  def thumbnails(conduit: ActorRef = IO(Http),
    root: String = "auto",
    path: String,
    format: Option[Format] = None,
    size: Option[Size] = None)(implicit timeout: Timeout = 15 minutes, maxChunkSize: Long = 1048576): Future[Tuple2[ContentMetadata, Stream[HttpData]]] = {
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
      sendReceive(conduit) ~>
      unmarshal[(ContentMetadata, Stream[HttpData])]
    )
    val q = Seq(format map ("format" -> _.toString), size map ("size" -> _.toString)) flatMap (f ⇒ f)
    pipeline {
      Get(Uri(s"https://api-content.dropbox.com/1/thumbnails/$root/$path") withQuery (q: _*))
    }
  }

  import scalaz.effect.IoExceptionOr
  import scalaz.iteratee.EnumeratorT
  import scalaz.effect.{ IO ⇒ zIO }
  def chunked_upload(conduit: ActorRef = IO(Http),
    contents: EnumeratorT[IoExceptionOr[(Array[Byte], Int)], zIO],
    idAndOffset: Option[Tuple2[String, Long]] = None)(implicit timeout: Timeout = 15 minutes): Future[UploadWithExpiry] = {
    import UploadWithExpiryJsonProtocol.uploadWithExpiryFormat
    import SprayJsonSupport.sprayJsonUnmarshaller
    import MetaMarshallers._
    import Arrays._

    implicit def boundArray2HttpData(t: (Array[Byte], Int)): HttpData = HttpData(t._1 takeT t._2)

    val pipeline = (
      addUserAgent ~>
      addAuthorization ~>
      sendReceive(conduit) ~>
      unmarshal[UploadWithExpiry]
    )
    val q = Seq(idAndOffset map ("upload_id" -> _._1), idAndOffset map ("offset" -> _._2.toString)) flatMap (f ⇒ f)
    pipeline {
      Put(Uri(s"https://api-content.dropbox.com/1/chunked_upload") withQuery (q: _*), contents)
    }
  }

  def commit_chunked_upload(conduit: ActorRef = IO(Http),
    root: String = "auto",
    path: String,
    upload_id: String,
    parent_rev: Option[String] = None,
    overwrite: Option[Boolean] = None)(implicit timeout: Timeout = 60 seconds, locale: Option[Locale] = None): Future[ContentMetadata] = {
    import ContentMetadataJsonProtocol.contentMetadataFormat
    import SprayJsonSupport.sprayJsonUnmarshaller

    val pipeline = (
      addUserAgent ~>
      addAuthorization ~>
      sendReceive(conduit) ~>
      unmarshal[ContentMetadata]
    )
    val q = Seq(Some("upload_id", upload_id), parent_rev map ("parent_rev" ->), overwrite map ("overwrite" -> _.toString), locale map ("locale" -> _.toLanguageTag)) flatMap (f ⇒ f)
    pipeline {
      Post(Uri(s"https://api-content.dropbox.com/1/commit_chunked_upload/$root/$path") withQuery (q: _*))
    }
  }

  def copy(conduit: ActorRef = IO(Http),
    root: String = "auto",
    to_path: String,
    from_path: Option[String] = None,
    from_copy_ref: Option[String] = None)(implicit timeout: Timeout = 60 seconds, locale: Option[Locale] = None): Future[ContentMetadata] = {

    import ContentMetadataJsonProtocol.contentMetadataFormat
    import SprayJsonSupport.sprayJsonUnmarshaller
    import spray.http.FormData

    val pipeline = (
      addUserAgent ~>
      addAuthorization ~>
      sendReceive(conduit) ~>
      unmarshal[ContentMetadata]
    )
    val payload = Seq(
      Some("root", root),
      Some("to_path", to_path),
      from_path map ("from_path" ->),
      from_copy_ref map ("from_copy_ref" ->),
      locale map ("locale" -> _.toLanguageTag)
    ) flatMap (f ⇒ f)

    pipeline {
      Post(Uri("https://api.dropbox.com/1/fileops/copy"), FormData(payload))
    }
  }

  def create_folder(conduit: ActorRef = IO(Http),
    root: String = "auto",
    path: String)(implicit timeout: Timeout = 60 seconds, locale: Option[Locale] = None): Future[ContentMetadata] = {

    import ContentMetadataJsonProtocol.contentMetadataFormat
    import SprayJsonSupport.sprayJsonUnmarshaller
    import spray.http.FormData

    val pipeline = (
      addUserAgent ~>
      addAuthorization ~>
      sendReceive(conduit) ~>
      unmarshal[ContentMetadata]
    )
    val payload = Seq(
      Some("root", root),
      Some("path", path),
      locale map ("locale" -> _.toLanguageTag)
    ) flatMap (f ⇒ f)

    pipeline {
      Post(Uri("https://api.dropbox.com/1/fileops/create_folder"), FormData(payload))
    }
  }

  def delete(conduit: ActorRef = IO(Http),
    root: String = "auto",
    path: String)(implicit timeout: Timeout = 60 seconds, locale: Option[Locale] = None): Future[ContentMetadata] = {

    import ContentMetadataJsonProtocol.contentMetadataFormat
    import SprayJsonSupport.sprayJsonUnmarshaller
    import spray.http.FormData

    val pipeline = (
      addUserAgent ~>
      addAuthorization ~>
      sendReceive(conduit) ~>
      unmarshal[ContentMetadata]
    )
    val payload = Seq(
      Some("root", root),
      Some("path", path),
      locale map ("locale" -> _.toLanguageTag)
    ) flatMap (f ⇒ f)

    pipeline {
      Post(Uri("https://api.dropbox.com/1/fileops/delete"), FormData(payload))
    }
  }

  def move(conduit: ActorRef = IO(Http),
    root: String = "auto",
    to_path: String,
    from_path: String)(implicit timeout: Timeout = 60 seconds, locale: Option[Locale] = None): Future[ContentMetadata] = {

    import ContentMetadataJsonProtocol.contentMetadataFormat
    import SprayJsonSupport.sprayJsonUnmarshaller
    import spray.http.FormData

    val pipeline = (
      addUserAgent ~>
      addAuthorization ~>
      sendReceive(conduit) ~>
      unmarshal[ContentMetadata]
    )
    val payload = Seq(
      Some("root", root),
      Some("to_path", to_path),
      Some("from_path", from_path),
      locale map ("locale" -> _.toLanguageTag)
    ) flatMap (f ⇒ f)

    pipeline {
      Post(Uri("https://api.dropbox.com/1/fileops/move"), FormData(payload))
    }
  }

  def shutdown(): Unit = {
    import akka.pattern.ask
    import spray.util.pimpFuture

    IO(Http).ask(Http.CloseAll)(3 seconds).await
    system shutdown
  }
}
