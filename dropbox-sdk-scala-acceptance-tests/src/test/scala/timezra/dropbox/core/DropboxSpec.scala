package timezra.dropbox.core

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import org.junit.runner.RunWith
import org.scalatest.BeforeAndAfter
import org.scalatest.FeatureSpec
import org.scalatest.GivenWhenThen
import org.scalatest.junit.JUnitRunner
import org.scalatest.Matchers
import com.typesafe.config.ConfigFactory
import spray.http.HttpData

@RunWith(classOf[JUnitRunner])
class DropboxSpec extends FeatureSpec with GivenWhenThen with BeforeAndAfter with Matchers {

  val config = ConfigFactory.load().getConfig("timezra.dropbox.core").getConfig("test").getConfig("client")
  val clientIdentifier = config.getString("clientIdentifier")
  val accessToken = config.getString("accessToken")

  var dropbox: Dropbox = _

  before {
    dropbox = Dropbox(clientIdentifier, accessToken)
  }

  after {
    dropbox shutdown
  }

  feature("Dropbox accounts") {
    scenario("Gets account info") {
      Given("An existing user")
      When("She requests her account info")
      val accountInfo = Await result (dropbox.accountInfo(), 3 second)

      Then("She should receive it")
      accountInfo.uid should be > 0L
      accountInfo.display_name shouldNot be(empty)
      accountInfo.quota_info shouldNot be(null)
    }
  }

  feature("Files and metadata") {
    scenario("Gets a file") {
      Given("A file in Dropbox") // TODO: upload the file to Dropbox
      val expectedContents = "Dropbox SDK Scala Test.\n"
      val path = "test.txt"
      When("A user downloads it")
      val response = Await result (dropbox.getFile(path = path), 3 second)

      Then("She should get its contents")
      val actualContents = response.foldLeft("")(_ + _.asString)
      actualContents shouldEqual expectedContents
    }
  }
}