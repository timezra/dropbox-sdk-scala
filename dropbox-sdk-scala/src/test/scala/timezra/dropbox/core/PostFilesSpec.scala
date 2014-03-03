package timezra.dropbox.core

import java.io.ByteArrayInputStream
import org.junit.runner.RunWith
import EnumeratorT.enumInputStream
import scalaz.effect.IO
import spray.http.ContentTypes.`application/octet-stream`
import spray.http.HttpEntity
import spray.http.HttpMethods.POST
import spray.http.HttpProtocols.`HTTP/1.1`
import spray.http.HttpRequest
import spray.http.Uri
import spray.http.StatusCodes.Unauthorized
import spray.http.StatusCodes.LengthRequired
import org.scalatest.junit.JUnitRunner
import java.text.DateFormat
import java.text.SimpleDateFormat
import spray.http.HttpResponse
import ContentTypes.`text/javascript`
import java.util.Locale
import spray.httpx.UnsuccessfulResponseException
import java.io.File
import spray.http.HttpData
import spray.http.HttpData.Compound
import spray.http.HttpData.FileBytes
import spray.http.ContentType
import spray.http.HttpData.Bytes
import spray.http.HttpHeaders.`Content-Disposition`

@RunWith(classOf[JUnitRunner])
class PostFilesSpec extends CoreSpec {

  val Root = "root"
  val Path = "subfolder"

  val Metadata = ContentMetadata("0 bytes", 0, "path", false, None, Some("rev"), None, false, "icon", Some(formatter.parse("Mon, 18 Jul 2011 20:13:43 +0000")), Some(formatter.parse("Wed, 20 Apr 2011 16:20:19 +0000")), "root", Some("text/plain"), Some(1), None)
  def formatter: DateFormat = new SimpleDateFormat("EEE, d MMM yyyy HH:mm:ss Z")
  val SuccessfulResponse = s"""
  {
      "size": "${Metadata.size}",
      "bytes": ${Metadata.bytes},
      "path": "${Metadata.path}",
      "is_dir": ${Metadata.is_dir},
      "rev": "${Metadata.rev.get}",
      "thumb_exists": ${Metadata.thumb_exists},
      "icon": "${Metadata.icon}",
      "modified": "${formatter.format(Metadata.modified.get)}",
      "client_mtime": "${formatter.format(Metadata.client_mtime.get)}",
      "root": "${Metadata.root}",
      "mime_type": "${Metadata.mime_type.get}",
      "revision": ${Metadata.revision.get}
  }
  """
  val LengthRequiredFailure = s"""
    {"error": "The request did not specify the length of its content."}
  """

  val thisFile = new File("src/test/scala/timezra/dropbox/core/PostFilesSpec.scala")

  describe("Files (POST)") {
    it("should make an http request") {
      val probe = ioProbe

      dropbox postFile (probe ref, Root, Path, thisFile)

      val request = probe expectMsgClass classOf[HttpRequest]

      request match {
        case HttpRequest(POST, uri, headers, HttpEntity.NonEmpty(ContentType(mediaType, _), Compound(Bytes(_), Compound(FileBytes(fileName, _, _), _))), `HTTP/1.1`) ⇒
          uri should be(Uri(s"https://api-content.dropbox.com/1/files/$Root/$Path"))
          headers should (contain(authorizationHeader) and contain(userAgentHeader))
          mediaType.isMultipart should be(true)
          new File(fileName).getCanonicalPath should be(thisFile getCanonicalPath)
      }
    }

    it("should set the Content-Disposition header") {
      val probe = ioProbe

      dropbox postFile (probe ref, Root, Path, thisFile)

      val request = probe expectMsgClass classOf[HttpRequest]

      request match {
        case HttpRequest(_, _, _, HttpEntity.NonEmpty(_, Compound(formHeader, _)), _) ⇒
          formHeader.asString should include(`Content-Disposition`("form-data", Map("name" -> "file", "filename" -> thisFile.getName)).toString)
      }
    }

    it("should return content metadata") {
      val probe = ioProbe

      val response = dropbox postFile (probe ref, path = Path, file = thisFile)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(entity = HttpEntity(`text/javascript`, SuccessfulResponse)))

      await(response) should be(Metadata)
    }

    it("should specify a parent revision") {
      val probe = ioProbe
      val parentRev = "1"

      val response = dropbox postFile (probe ref, path = Path, file = thisFile, parent_rev = Some(parentRev))

      val request = probe expectMsgClass classOf[HttpRequest]

      request.uri.query.get("parent_rev").get should be(parentRev)
    }

    it("should specify whether to overwrite an existing file") {
      val probe = ioProbe
      val overwrite = false

      val response = dropbox postFile (probe ref, path = Path, file = thisFile, overwrite = Some(overwrite))

      val request = probe expectMsgClass classOf[HttpRequest]

      request.uri.query.get("overwrite").get.toBoolean should be(overwrite)
    }

    it("should ask for language specific text") {
      val probe = ioProbe

      implicit val locale = Some(Locale.CHINA)
      val response = dropbox postFile (probe ref, path = Path, file = thisFile)

      val request = probe expectMsgClass classOf[HttpRequest]

      request.uri.query.get("locale") should be(locale.map(_.toLanguageTag))
    }

    it("should propagate authorization failures") {
      val probe = ioProbe

      val response = dropbox postFile (probe ref, path = Path, file = thisFile)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(Unauthorized, HttpEntity(`text/javascript`, AuthorizationFailure)))

      val error = intercept[UnsuccessfulResponseException] { await(response) }
      error.getMessage() should be(s"Status: $Unauthorized\nBody: $AuthorizationFailure")
    }

    it("should propagate missing content-length header failures") {
      val probe = ioProbe

      val response = dropbox postFile (probe ref, path = Path, file = thisFile)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(LengthRequired, HttpEntity(`text/javascript`, LengthRequiredFailure)))

      val error = intercept[UnsuccessfulResponseException] { await(response) }
      error.getMessage() should be(s"Status: $LengthRequired\nBody: $LengthRequiredFailure")
    }
  }
}