package timezra.dropbox.core

import java.io.{ File ⇒ JFile }
import scala.reflect.io.{ File ⇒ SFile }
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import org.junit.runner.RunWith
import org.scalatest.BeforeAndAfterAll
import org.scalatest.FeatureSpec
import org.scalatest.GivenWhenThen
import org.scalatest.junit.JUnitRunner
import org.scalatest.Matchers
import com.typesafe.config.ConfigFactory
import org.scalatest.ConfigMap
import java.util.UUID
import org.scalatest.Inside
import java.util.Date
import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class DropboxSpec extends FeatureSpec with GivenWhenThen with BeforeAndAfterAll with Matchers with Inside {

  val config = ConfigFactory.load().getConfig("timezra.dropbox.core").getConfig("test").getConfig("client")
  val clientIdentifier = config.getString("clientIdentifier")
  val accessToken = config.getString("accessToken")

  lazy val dropbox: Dropbox = Dropbox(clientIdentifier, accessToken)

  override def afterAll(configMap: ConfigMap) {
    super.afterAll(configMap)
    dropbox shutdown
  }

  feature("Dropbox accounts") {
    scenario("Gets account info") {
      Given("An existing user")

      When("She requests her account info")
      val accountInfo = Await result (dropbox accountInfo (), 3 seconds)

      Then("She should receive it")
      accountInfo.uid should be > 0L
      accountInfo.display_name should not be (empty)
      accountInfo.quota_info should not be (null)
    }
  }

  feature("Files and metadata") {
    scenario("Gets a file") {
      Given("A file in Dropbox") // TODO: upload the file to Dropbox
      val expectedContents = "Dropbox SDK Scala Test.\n"
      val path = "test.txt"

      When("A user downloads it")
      val response = Await result (dropbox getFile (path = path), 3 seconds)

      Then("She should get its contents")
      val actualContents = response._2.foldLeft("")(_ + _.asString)
      actualContents should be(expectedContents)
      And("Its metadata")
      val contentMetadata = response._1
      contentMetadata.is_dir should be(false)
      contentMetadata.bytes should be > 0L
      contentMetadata.path should be(s"/$path")
    }

    scenario("Puts a file") {
      import Implicits._

      Given("Some file contents")
      val expectedContents = "Dropbox SDK Scala Test.\n"
      val path = "test.txt"

      When("A user puts them in Dropbox")
      Await result (dropbox putFile (path = path, contents = expectedContents, length = expectedContents length), 3 seconds)

      Then("That file should be in Dropbox")
      val response = Await result (dropbox getFile (path = path), 3 seconds)
      val actualContents = response._2.foldLeft("")(_ + _.asString)
      actualContents should be(expectedContents)
      // TODO: Delete the file from Dropbox
    }

    scenario("Posts a file") {
      Given("A local file")
      val expectedFile = new JFile("src/test/resources/application.conf")
      val path = ""
      val expectedFilename = "test_post.txt"

      When("A user posts it to Dropbox")
      Await result (dropbox postFile (path = path, file = expectedFile, filename = Some(expectedFilename)), 3 seconds)

      Then("That file should be in Dropbox")
      val response = Await result (dropbox getFile (path = s"$path/$expectedFilename"), 3 seconds)
      val actualContents = response._2.foldLeft("")(_ + _.asString)
      actualContents should be(new SFile(expectedFile).slurp)
      // TODO: Delete the file from Dropbox
    }

    scenario("Gets File Metadata") {
      Given("A file in Dropbox") // TODO: upload the file to Dropbox
      val path = "test.txt"

      When("A user gets its metadata")
      val response = Await result (dropbox metadata (path = path), 3 seconds)

      Then("She should receive them")
      val contentMetadata = response.right.get
      contentMetadata.is_dir should be(false)
      contentMetadata.bytes should be > 0L
      contentMetadata.path should be(s"/$path")
    }

    scenario("Gets Folder Metadata") {
      Given("A folder in Dropbox")
      val path = ""

      When("A user gets its metadata")
      val response = Await result (dropbox metadata (path = path), 3 seconds)

      Then("She should receive them")
      val contentMetadata = response.right.get
      contentMetadata.is_dir should be(true)
      contentMetadata.bytes should be(0L)
      contentMetadata.path should be(s"/$path")
      contentMetadata.contents.isDefined should be(true)
    }

    scenario("Gets A Change Delta") {
      Given("A folder in Dropbox")
      val path_prefix = "/"

      When("A user asks for its delta")
      val deltaMetadata = Await result (dropbox delta (path_prefix = Some(path_prefix)), 3 seconds)

      Then("She should receive it")
      deltaMetadata.entries should not be (null)
      deltaMetadata.reset should be(true)
      deltaMetadata.has_more should be(false)
      deltaMetadata.cursor should not be (null)
    }

    scenario("Gets A Longpoll Delta") {
      import Implicits._

      Given("A cursor")
      val dropboxMetadata = (Await result (dropbox delta (path_prefix = Some("/")), 3 seconds))

      When("A user polls for a change")
      val longpoll_result = dropbox longpoll_delta (cursor = dropboxMetadata.cursor)

      And("Modifies the contents of the folder")
      val path = UUID.randomUUID().toString
      val contents = "Longpoll Delta Test"
      Await result (dropbox putFile (path = path, contents = contents, length = contents length), 3 seconds)

      Then("The user should get an indication that the folder contents have changed")
      val longpollMetadata = Await result (longpoll_result, 3 seconds)
      longpollMetadata.changes should be(true)

      // TODO: Delete the file from Dropbox
    }

    scenario("Gets Revisions") {
      Given("A file in Dropbox") // TODO: upload the file to Dropbox
      val filePath = "test.txt"

      When("A user gets its revisions")
      val revisions = Await result (dropbox revisions (path = filePath), 3 seconds)

      Then("She should receive them")
      revisions.size should be > 0
      revisions.foreach(revision ⇒
        inside(revision) {
          case ContentMetadata(_, bytes, path, is_dir, _, _, _, _, _, _, _, _, _, _, _) ⇒
            bytes should be >= 0L
            is_dir should be(false)
            path should be(s"/$filePath")
        }
      )
    }

    scenario("Restores a File to a Previous Revision") {
      import Implicits._

      Given("A file in Dropbox")
      val path = UUID.randomUUID().toString
      val originalContents = "Original Contents"
      Await result (dropbox putFile (path = path, contents = originalContents, length = originalContents length), 3 seconds)

      When("A user changes it")
      val newContents = "New Contents"
      Await result (dropbox putFile (path = path, contents = newContents, length = newContents length), 3 seconds)

      And("Restores it to the previous revision")
      val revisions = Await result (dropbox revisions (path = path), 3 seconds)
      val theFirstRevision = revisions.last
      val revision = theFirstRevision.revision.get
      val rev = theFirstRevision.rev.get
      val contentMetadata: ContentMetadata = Await result (dropbox restore (path = path, rev = rev), 3 seconds)

      Then("It should have its original content")
      contentMetadata.revision.get should be > revision
      val response = Await result (dropbox getFile (path = path), 3 seconds)
      val actualContents = response._2.foldLeft("")(_ + _.asString)
      actualContents should be(originalContents)

      // TODO: delete the file from Dropbox
    }

    scenario("Searches For Matching Files") {
      import Implicits._

      Given("A query")
      val query = ".search_test"

      And("A file that matches the query")
      val path = UUID.randomUUID().toString + query
      val contents = "Search Test"
      Await result (dropbox putFile (path = path, contents = contents, length = contents length), 3 seconds)

      When("A user searches with the query")
      val metadata = Await result (dropbox search (query = query), 3 seconds)

      Then("She should get metadata for any matching files")
      metadata.size should be > 0
      metadata.foreach(metadatum ⇒
        inside(metadatum) {
          case ContentMetadata(_, bytes, path, is_dir, _, _, _, _, _, _, _, _, _, _, _) ⇒
            path should include(query)
        }
      )

      // TODO: delete the files from Dropbox
    }

    scenario("Shares A File") {
      Given("A file in Dropbox") // TODO: upload the file to Dropbox
      val path = "test.txt"

      When("A user shares it")
      val sharesLinkWithExpiry = Await result (dropbox shares (path = path), 3 seconds)

      Then("She should get a url and expiration date for it")
      sharesLinkWithExpiry.url should not be (null)
      sharesLinkWithExpiry.expires should be > new Date()
    }

    scenario("Asks For A Media Link") {
      Given("A file in Dropbox") // TODO: upload the file to Dropbox
      val path = "test.txt"

      When("A user asks for a media link to it")
      val mediaLinkWithExpiry = Await result (dropbox media (path = path), 3 seconds)

      Then("She should get a url and expiration date for it")
      mediaLinkWithExpiry.url should not be (null)
      mediaLinkWithExpiry.expires should be > new Date()
    }

    scenario("Asks For A Copy Reference") {
      Given("A file in Dropbox") // TODO: upload the file to Dropbox
      val path = "test.txt"

      When("A user asks for a reference to copy it")
      val referenceWithExpiry = Await result (dropbox copy_ref (path = path), 3 seconds)

      Then("She should get an id and expiration date for it")
      referenceWithExpiry.copy_ref should not be (null)
      referenceWithExpiry.expires should be > new Date()
    }

    scenario("Asks For A Thumbnail") {
      Given("A picture in Dropbox") // TODO: upload the file to Dropbox
      val path = "test.png"

      When("A user asks for a thumbnail of it")
      val response = Await result (dropbox thumbnails (path = path), 3 seconds)

      Then("She should get its contents")
      val actualContents: ArrayBuffer[Byte] = response._2.foldLeft(new ArrayBuffer[Byte]())(_ ++= _.toByteArray)
      actualContents.length should be > 0
      And("Its metadata")
      val contentMetadata = response._1
      contentMetadata.is_dir should be(false)
      contentMetadata.bytes should be > 0L
    }

    scenario("Uploads a file in chunks") {
      import Implicits._

      Given("Some file contents")
      val theFirstChunk = "Dropbox SDK"
      val theSecondChunk = " Scala Test.\n"
      val path = "test_chunk.txt"

      When("A user uploads part of the file")
      val response = Await result (dropbox chunked_upload (contents = theFirstChunk), 3 seconds)

      And("Uploads the rest of the file")
      val uploadId = response.upload_id
      val offset = response.offset
      Await result (dropbox chunked_upload (contents = theSecondChunk, idAndOffset = Some(uploadId, offset)), 3 seconds)

      And("Commits the file upload")
      Await result (dropbox commit_chunked_upload (path = path, upload_id = uploadId), 3 seconds)

      Then("That file should be in Dropbox")
      val actualContents = (Await result (dropbox getFile (path = path), 3 seconds))._2.foldLeft("")(_ + _.asString)
      actualContents should be(theFirstChunk + theSecondChunk)
      // TODO: Delete the file from Dropbox
    }
  }
}