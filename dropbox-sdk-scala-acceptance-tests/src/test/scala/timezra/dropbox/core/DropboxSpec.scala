package timezra.dropbox.core

import org.scalatest.FeatureSpec
import org.scalatest.GivenWhenThen
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class DropboxSpec extends FeatureSpec with GivenWhenThen {

  feature("Account Info") {
    scenario("Gets Account Info") {
      Given("A Dropbox Client")

      When("Existing users request their account info")

      Then("They should receive it")

      pending
    }
  }
}