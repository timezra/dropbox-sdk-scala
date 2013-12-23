package timezra.dropbox.core

import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ByteRangeSpec extends FunSpec with Matchers {

  describe("ByteRange") {
    it("should take just a start") {
      ByteRange(Some(0), None).toString should be("0-")
    }

    it("should take just an end") {
      ByteRange(None, Some(10)).toString should be("-10")
    }

    it("should take a start and an end") {
      ByteRange(Some(0), Some(10)).toString should be("0-10")
    }

    it("should not allow a missing start and end") {
      evaluating { ByteRange(None, None) } should produce[IllegalArgumentException]
    }
  }
}