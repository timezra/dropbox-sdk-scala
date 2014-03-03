package timezra.dropbox.core

import java.text.DateFormat
import java.text.SimpleDateFormat
import java.util.Locale
import org.junit.runner.RunWith
import spray.http.HttpEntity
import spray.http.HttpRequest
import spray.http.HttpResponse
import spray.http.StatusCodes
import spray.http.Uri.apply
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MetadataSpec extends CoreSpec {

  val Root = "root"
  val FilePath = "test.txt"
  val FolderPath = "subfolder"
  val FileLimit = 25000
  val Hash = "hash"
  val Rev = "test"

  val FileMetadata = ContentMetadata("10 bytes", 10, s"/$FilePath", false, None, Some("rev"), None, false, "fileIcon", Some(formatter.parse("Mon, 18 Jul 2011 20:13:43 +0000")), Some(formatter.parse("Wed, 20 Apr 2011 16:20:19 +0000")), "root", Some("mime_type"), Some(1), None)
  val FolderMetadata = ContentMetadata("0 bytes", 0, s"/$FolderPath", true, None, None, Some(Hash), false, "folderIcon", None, None, "root", None, None, Some(List(FileMetadata)))
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

  val FolderMetadataJson = s"""
  {
      "hash": "${FolderMetadata.hash.get}",
      "thumb_exists": ${FolderMetadata.thumb_exists},
      "bytes": ${FolderMetadata.bytes},
      "path": "${FolderMetadata.path}", 
      "is_dir": ${FolderMetadata.is_dir}, 
      "size": "${FolderMetadata.size}", 
      "root": "${FolderMetadata.root}", 
      "contents": [$FileMetadataJson],
      "icon": "${FolderMetadata.icon}"
  }
  """

  describe("Metadata") {

    it("should make an http request") {
      val probe = ioProbe

      dropbox metadata (probe ref, Root, FilePath)

      val expectedURI = s"https://api.dropbox.com/1/metadata/$Root/$FilePath"
      probe expectMsg HttpRequest(uri = expectedURI, headers = List(authorizationHeader, userAgentHeader))
    }

    it("should request a specific file limit") {
      val probe = ioProbe

      dropbox metadata (probe ref, Root, FilePath, Some(FileLimit))

      val request = probe expectMsgClass classOf[HttpRequest]
      request.uri.query.get("file_limit").get.toInt should be(FileLimit)
    }

    it("should request a specific hash") {
      val probe = ioProbe

      dropbox metadata (probe ref, Root, FilePath, hash = Some(Hash))

      val request = probe expectMsgClass classOf[HttpRequest]
      request.uri.query.get("hash").get should be(Hash)
    }

    it("should request that contents be listed") {
      val probe = ioProbe

      dropbox metadata (probe ref, Root, FilePath, list = Some(true))

      val request = probe expectMsgClass classOf[HttpRequest]
      request.uri.query.get("list").get.toBoolean should be(true)
    }

    it("should request that deleted contents be listed") {
      val probe = ioProbe

      dropbox metadata (probe ref, Root, FilePath, include_deleted = Some(true))

      val request = probe expectMsgClass classOf[HttpRequest]
      request.uri.query.get("include_deleted").get.toBoolean should be(true)
    }

    it("should request a specific revision") {
      val probe = ioProbe

      dropbox metadata (probe ref, Root, FilePath, rev = Some(Rev))

      val request = probe expectMsgClass classOf[HttpRequest]
      request.uri.query.get("rev").get should be(Rev)
    }

    it("should request language specific text") {
      val probe = ioProbe

      implicit val locale = Some(Locale.CHINA)
      dropbox metadata (probe ref, Root, FilePath)

      val request = probe expectMsgClass classOf[HttpRequest]

      request.uri.query.get("locale") should be(locale.map(_.toLanguageTag))
    }

    it("should return true if folder contents have not changed") {
      val probe = ioProbe

      val response = dropbox metadata (probe ref, Root, FolderPath, hash = Some(Hash))

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(StatusCodes.NotModified))

      await(response) should be(Left(true))
    }

    it("should return file content metadata") {
      val probe = ioProbe

      val response = dropbox metadata (probe ref, Root, FilePath)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(StatusCodes.OK, HttpEntity(ContentTypes `text/javascript`, FileMetadataJson)))

      val either = await(response)

      either should be(Right(FileMetadata))
    }

    it("should return folder content metadata") {
      val probe = ioProbe

      val response = dropbox metadata (probe ref, Root, FolderPath)

      probe expectMsgClass classOf[HttpRequest]
      probe reply (HttpResponse(StatusCodes.OK, HttpEntity(ContentTypes `text/javascript`, FolderMetadataJson)))

      val either = await(response)

      either should be(Right(FolderMetadata))
    }
  }
}