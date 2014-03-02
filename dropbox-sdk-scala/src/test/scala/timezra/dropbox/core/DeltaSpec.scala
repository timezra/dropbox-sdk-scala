package timezra.dropbox.core

import java.text.DateFormat
import java.text.SimpleDateFormat
import java.util.Locale
import org.junit.runner.RunWith
import spray.http.ContentType.apply
import spray.http.HttpData.Bytes
import spray.http.HttpEntity
import spray.http.HttpMethods.POST
import spray.http.HttpRequest
import spray.http.HttpResponse
import spray.http.StatusCodes
import spray.http.Uri.apply
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class DeltaSpec extends CoreSpec {

  val FilePath = "test.txt"
  val Reset = true
  val Cursor = "cursor"
  val HasMore = false
  val PathPrefix = "/"

  val FileMetadata = ContentMetadata("10 bytes", 10, s"/$FilePath", false, None, Some("rev"), None, false, "fileIcon", Some(formatter.parse("Mon, 18 Jul 2011 20:13:43 +0000")), Some(formatter.parse("Wed, 20 Apr 2011 16:20:19 +0000")), "root", Some("mime_type"), Some(1), None)
  val DeltaEntry = Tuple2(s"/$FilePath", FileMetadata)
  val Metadata = DeltaMetadata(List(DeltaEntry), Reset, Cursor, HasMore)
  def formatter: DateFormat = new SimpleDateFormat("EEE, d MMM yyyy HH:mm:ss Z")

  val FileMetadataJson = s"""
  {
      "size": "${FileMetadata.size}",
      "bytes": ${FileMetadata.bytes},
      "path": "${FileMetadata.path}",
      "is_dir": ${FileMetadata.is_dir},
      "rev": "${FileMetadata.rev.get}",
      "thumb_exists": ${FileMetadata.thumb_exists},
      "icon": "${FileMetadata.icon}",
      "modified": "${formatter.format(FileMetadata.modified.get)}",
      "client_mtime": "${formatter.format(FileMetadata.client_mtime.get)}",
      "root": "${FileMetadata.root}",
      "mime_type": "${FileMetadata.mime_type.get}",
      "revision": ${FileMetadata.revision.get}
  }
  """

  val DeltaMetadataJson = s"""
  {
      "entries": [["/$FilePath", $FileMetadataJson]],
      "reset": $Reset,
      "cursor": "$Cursor",
      "has_more": $HasMore
  }
  """

  describe("Delta") {

    it("should make an http request") {
      val probe = ioProbe

      dropbox delta (probe ref)

      val expectedURI = "https://api.dropbox.com/1/delta"
      probe expectMsg HttpRequest(method = POST, uri = expectedURI, headers = List(authorizationHeader, userAgentHeader))
    }

    it("should request a specific path prefix") {
      val probe = ioProbe

      dropbox delta (probe ref, Some(PathPrefix))

      val request = probe expectMsgClass classOf[HttpRequest]

      request match {
        case HttpRequest(POST, _, _, HttpEntity.NonEmpty(_, Bytes(byteString)), _) ⇒
          byteString.utf8String should be(s"path_prefix=$PathPrefix")
      }
    }

    it("should request a specific cursor") {
      val probe = ioProbe

      dropbox delta (probe ref, cursor = Some(Cursor))

      val request = probe expectMsgClass classOf[HttpRequest]

      request match {
        case HttpRequest(POST, _, _, HttpEntity.NonEmpty(_, Bytes(byteString)), _) ⇒
          byteString.utf8String should be(s"cursor=$Cursor")
      }
    }

    it("should request language specific text") {
      val probe = ioProbe

      implicit val locale = Some(Locale.CHINA)
      dropbox delta (probe ref)

      val request = probe expectMsgClass classOf[HttpRequest]

      request match {
        case HttpRequest(POST, _, _, HttpEntity.NonEmpty(_, Bytes(byteString)), _) ⇒
          byteString.utf8String should be(s"locale=${locale.get.toLanguageTag}")
      }
    }

    it("should return delta metadata") {
      val probe = ioProbe

      val response = dropbox delta (probe ref)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(StatusCodes.OK, HttpEntity(ContentTypes `text/javascript`, DeltaMetadataJson)))

      await(response) should be(Metadata)
    }
  }
}