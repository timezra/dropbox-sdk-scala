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
import scala.util.Try

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
      val accountInfo = Await result (dropbox accountInfo (), 5 seconds)

      Then("She should receive it")
      accountInfo.uid should be > 0L
      accountInfo.display_name should not be (empty)
      accountInfo.quota_info should not be (null)
    }
  }

  feature("Files and metadata") {
    scenario("Gets a file") {
      Given("A file in Dropbox")
      withDropboxFile {
        (path, contents) ⇒
          {
            When("A user downloads it")
            val response = Await result (dropbox getFile (path = path), 5 seconds)

            Then("She should get its contents")
            val actualContents = response._2.foldLeft("")(_ + _.asString)
            actualContents should be(contents)
            And("Its metadata")
            val contentMetadata = response._1
            contentMetadata.is_dir should be(false)
            contentMetadata.bytes should be > 0L
            contentMetadata.path should be(s"/$path")
          }
      }
    }

    scenario("Puts a file") {
      Given("Some file contents")
      val expectedContents = "Dropbox SDK Scala Test.\n"

      When("A user puts them in Dropbox")
      withDropboxResource {
        path ⇒
          {
            import Implicits._
            Await result (dropbox putFile (path = path, contents = expectedContents, length = expectedContents length), 5 seconds)

            Then("That file should be in Dropbox")
            val response = Await result (dropbox getFile (path = path), 5 seconds)
            val actualContents = response._2.foldLeft("")(_ + _.asString)
            actualContents should be(expectedContents)
          }
      }
    }

    scenario("Posts a file") {
      Given("A local file")
      val expectedFile = new JFile("src/test/resources/application.conf")
      val path = ""

      When("A user posts it to Dropbox")
      withDropboxResource {
        expectedFilename ⇒
          {
            Await result (dropbox postFile (path = path, file = expectedFile, filename = Some(expectedFilename)), 5 seconds)

            Then("That file should be in Dropbox")
            val response = Await result (dropbox getFile (path = s"$path/$expectedFilename"), 5 seconds)
            val actualContents = response._2.foldLeft("")(_ + _.asString)
            actualContents should be(new SFile(expectedFile).slurp)
          }
      }
    }

    scenario("Gets File Metadata") {
      Given("A file in Dropbox")
      withDropboxFile {
        (path, contents) ⇒
          {
            When("A user gets its metadata")
            val response = Await result (dropbox metadata (path = path), 5 seconds)

            Then("She should receive them")
            val contentMetadata = response.right.get
            contentMetadata.is_dir should be(false)
            contentMetadata.bytes should be > 0L
            contentMetadata.path should be(s"/$path")
          }
      }
    }

    scenario("Gets Folder Metadata") {
      Given("A folder in Dropbox")
      val path = ""

      When("A user gets its metadata")
      val response = Await result (dropbox metadata (path = path), 5 seconds)

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
      val deltaMetadata = Await result (dropbox delta (path_prefix = Some(path_prefix)), 5 seconds)

      Then("She should receive it")
      deltaMetadata.entries should not be (null)
      deltaMetadata.reset should be(true)
      deltaMetadata.has_more should be(false)
      deltaMetadata.cursor should not be (null)
    }

    scenario("Gets A Longpoll Delta") {
      Given("A cursor")
      val dropboxMetadata = (Await result (dropbox delta (path_prefix = Some("/")), 5 seconds))

      When("A user polls for a change")
      val longpoll_result = dropbox longpoll_delta (cursor = dropboxMetadata.cursor)

      And("Modifies the contents of the folder")
      withDropboxFile {
        (path, contents) ⇒
          {
            Then("The user should get an indication that the folder contents have changed")
            val longpollMetadata = Await result (longpoll_result, 5 seconds)
            longpollMetadata.changes should be(true)
          }
      }
    }

    scenario("Gets Revisions") {
      Given("A file in Dropbox")
      withDropboxFile {
        (path, contents) ⇒
          {
            When("A user gets its revisions")
            val revisions = Await result (dropbox revisions (path = path), 5 seconds)

            Then("She should receive them")
            revisions.size should be > 0
            revisions.foreach(revision ⇒
              inside(revision) {
                case ContentMetadata(_, bytes, filePath, is_dir, _, _, _, _, _, _, _, _, _, _, _) ⇒
                  bytes should be >= 0L
                  is_dir should be(false)
                  filePath should be(s"/$path")
              }
            )
          }
      }
    }

    scenario("Restores a File to a Previous Revision") {
      Given("A file in Dropbox")
      withDropboxFile {
        (path, contents) ⇒
          {
            import Implicits._

            When("A user changes it")
            val newContents = "New Contents"
            Await result (dropbox putFile (path = path, contents = newContents, length = newContents length), 5 seconds)

            And("Restores it to the previous revision")
            val revisions = Await result (dropbox revisions (path = path), 5 seconds)
            val theFirstRevision = revisions.last
            val revision = theFirstRevision.revision.get
            val rev = theFirstRevision.rev.get
            val contentMetadata: ContentMetadata = Await result (dropbox restore (path = path, rev = rev), 5 seconds)

            Then("It should have its original content")
            contentMetadata.revision.get should be > revision
            val response = Await result (dropbox getFile (path = path), 5 seconds)
            val actualContents = response._2.foldLeft("")(_ + _.asString)
            actualContents should be(contents)
          }
      }
    }

    scenario("Searches For Matching Files") {
      Given("A file in Dropbox")
      withDropboxFile {
        (path, contents) ⇒
          {
            And("A query that matches the file")
            val query = path.substring(2, 6)

            When("A user searches with the query")
            val metadata = Await result (dropbox search (query = query), 5 seconds)

            Then("She should get metadata for any matching files")
            metadata.size should be > 0
            metadata.foreach(metadatum ⇒
              inside(metadatum) {
                case ContentMetadata(_, bytes, path, is_dir, _, _, _, _, _, _, _, _, _, _, _) ⇒
                  path should include(query)
              }
            )
          }
      }
    }

    scenario("Shares A File") {
      Given("A file in Dropbox")
      withDropboxFile {
        (path, contents) ⇒
          {
            When("A user shares it")
            val sharesLinkWithExpiry = Await result (dropbox shares (path = path), 5 seconds)

            Then("She should get a url and expiration date for it")
            sharesLinkWithExpiry.url should not be (null)
            sharesLinkWithExpiry.expires should be > new Date()
          }
      }
    }

    scenario("Asks For A Media Link") {
      Given("A file in Dropbox")
      withDropboxFile {
        (path, contents) ⇒
          {
            When("A user asks for a media link to it")
            val mediaLinkWithExpiry = Await result (dropbox media (path = path), 5 seconds)

            Then("She should get a url and expiration date for it")
            mediaLinkWithExpiry.url should not be (null)
            mediaLinkWithExpiry.expires should be > new Date()
          }
      }
    }

    scenario("Asks For A Copy Reference") {
      Given("A file in Dropbox")
      withDropboxFile {
        (path, contents) ⇒
          {
            When("A user asks for a reference to copy it")
            val referenceWithExpiry = Await result (dropbox copy_ref (path = path), 5 seconds)

            Then("She should get an id and expiration date for it")
            referenceWithExpiry.copy_ref should not be (null)
            referenceWithExpiry.expires should be > new Date()
          }
      }
    }

    scenario("Asks For A Thumbnail") {
      Given("A picture in Dropbox")
      withDropboxPicture({
        (path) ⇒
          {
            When("A user asks for a thumbnail of it")
            val response = Await result (dropbox thumbnails (path = path), 5 seconds)

            Then("She should get its contents")
            val actualContents: ArrayBuffer[Byte] = response._2.foldLeft(new ArrayBuffer[Byte]())(_ ++= _.toByteArray)
            actualContents.length should be > 0
            And("Its metadata")
            val contentMetadata = response._1
            contentMetadata.is_dir should be(false)
            contentMetadata.bytes should be > 0L
          }
      }, Some("png"))
    }

    scenario("Uploads a file in chunks") {
      import Implicits._

      Given("Some file contents")
      val theFirstChunk = "Dropbox SDK"
      val theSecondChunk = " Scala Test.\n"

      When("A user uploads part of the file")
      val response = Await result (dropbox chunked_upload (contents = theFirstChunk), 5 seconds)

      And("Uploads the rest of the file")
      val uploadId = response.upload_id
      val offset = response.offset
      Await result (dropbox chunked_upload (contents = theSecondChunk, idAndOffset = Some(uploadId, offset)), 5 seconds)

      And("Commits the file upload")
      withDropboxResource {
        path ⇒
          {
            Await result (dropbox commit_chunked_upload (path = path, upload_id = uploadId), 5 seconds)

            Then("That file should be in Dropbox")
            val actualContents = (Await result (dropbox getFile (path = path), 5 seconds))._2.foldLeft("")(_ + _.asString)
            actualContents should be(theFirstChunk + theSecondChunk)
          }
      }
    }
  }

  feature("File operations") {
    scenario("Copies a file") {
      Given("A file in Dropbox")
      withDropboxFile {
        (copyFromPath, contents) ⇒
          {
            When("A user copies it")
            withDropboxResource {
              copyToPath ⇒
                {
                  Await result (dropbox copy (to_path = copyToPath, from_path = Some(copyFromPath)), 5 seconds)

                  Then("The copied file should be the same as the original")
                  val originalContents = (Await result (dropbox getFile (path = copyFromPath), 5 seconds))._2.foldLeft("")(_ + _.asString)
                  val copiedContents = (Await result (dropbox getFile (path = copyFromPath), 5 seconds))._2.foldLeft("")(_ + _.asString)
                  copiedContents should be(originalContents)
                }
            }
          }
      }
    }

    scenario("Copies a file from a reference") {
      Given("A reference to a file in Dropbox")
      withDropboxFile {
        (copyFromPath, contents) ⇒
          {
            val referenceWithExpiry = Await result (dropbox copy_ref (path = copyFromPath), 5 seconds)

            When("A user copies it")
            withDropboxResource {
              copyToPath ⇒
                {
                  Await result (dropbox copy (to_path = copyToPath, from_copy_ref = Some(referenceWithExpiry.copy_ref)), 5 seconds)

                  Then("The copied file should be the same as the original")
                  val originalContents = (Await result (dropbox getFile (path = copyFromPath), 5 seconds))._2.foldLeft("")(_ + _.asString)
                  val copiedContents = (Await result (dropbox getFile (path = copyFromPath), 5 seconds))._2.foldLeft("")(_ + _.asString)
                  copiedContents should be(originalContents)
                }
            }
          }
      }
    }

    scenario("Creates a folder") {
      withDropboxResource {
        path ⇒
          {
            Given("A Dropbox root folder")

            When("A user creates a folder relative to it")
            val response = Await result (dropbox create_folder (path = path), 5 seconds)

            Then("The folder should exist")
            response.is_dir should be(true)
            response.path should be(s"/$path")
            response.contents.isDefined should be(false)
          }
      }
    }

    scenario("Deletes a file") {
      Given("A file in Dropbox")
      withDropboxFile {
        (path, contents) ⇒
          {
            When("A user deletes it")
            val response = Await result (dropbox delete (path = path), 5 seconds)

            Then("The file should no longer exist")
            response.is_deleted.get should be(true)
            response.bytes should be(0)
            response.path should be(s"/$path")
            response.is_dir should be(false)
          }
      }
    }

    scenario("Moves a file") {
      Given("A file in Dropbox")
      withDropboxFile {
        (moveFromPath, contents) ⇒
          {
            When("A user moves it")
            withDropboxResource {
              moveToPath ⇒
                {
                  Await result (dropbox move (from_path = moveFromPath, to_path = moveToPath), 5 seconds)

                  Then("The file should no longer exist at the original location")
                  val moveFromMetadata = (Await result (dropbox metadata (path = moveFromPath), 5 seconds)).right.get
                  val moveToMetadata = (Await result (dropbox metadata (path = moveToPath), 5 seconds)).right.get
                  moveFromMetadata.bytes should be(0)
                  moveFromMetadata.path should be(s"/$moveFromPath")
                  moveToMetadata.bytes should be > 0L
                  moveToMetadata.path should be(s"/$moveToPath")
                }
            }
          }
      }
    }
  }

  private def withDropboxResource(test: String ⇒ Unit, extension: Option[String] = None) {
    import scala.util.control.Exception._
    val path = Seq(Some(UUID.randomUUID().toString), extension).flatMap(f ⇒ f).mkString(".")

    ultimately(dropbox delete (path = path)) apply test(path)
  }

  private def withDropboxFile(test: (String, String) ⇒ Unit, extension: Option[String] = None) {
    withDropboxResource({
      path ⇒
        {
          import Implicits._
          val contents = "Dropbox SDK Scala Test.\n"
          Await result (dropbox putFile (path = path, contents = contents, length = contents length), 5 seconds)
          test(path, contents)
        }
    }, extension)
  }

  private def withDropboxPicture(test: (String) ⇒ Unit, extension: Option[String] = None) {
    withDropboxResource({
      path ⇒
        {
          val pictureFile = new JFile("src/test/resources/test.png")
          val root = ""
          Await result (dropbox postFile (path = root, file = pictureFile, filename = Some(path)), 5 seconds)

          test(path)
        }
    }, extension)
  }
}