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

  feature("Account Info") {
    scenario("Gets Account Info") {
      Given("A Dropbox Client")
      val dropbox = new Dropbox(clientIdentifier, accessToken)

      When("Existing users request their account info")
      val accountInfo = Await result (dropbox.accountInfo(), 3 second)

      Then("They should receive it")
      accountInfo.uid should be > 0L
      accountInfo.display_name shouldNot be(empty)
      accountInfo.quota_info shouldNot be(null)
    }
  }
}