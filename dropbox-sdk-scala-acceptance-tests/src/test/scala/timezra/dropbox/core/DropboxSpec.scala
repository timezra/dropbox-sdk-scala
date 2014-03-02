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

@RunWith(classOf[JUnitRunner])
class DropboxSpec extends FeatureSpec with GivenWhenThen with BeforeAndAfterAll with Matchers {

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
      When("A user gets its delta")
      val deltaMetadata = Await result (dropbox delta (path_prefix = Some(path_prefix)), 3 seconds)
      And("A user gets its delta")

      Then("She should receive them")
      deltaMetadata.entries should not be (null)
      deltaMetadata.reset should be(true)
      deltaMetadata.has_more should be(false)
      deltaMetadata.cursor should not be (null)
    }
  }
}