package pts

import org.scalatest._
import scala.util.parsing.input._

class SourceInfoSpec extends FunSpec with Matchers {
  describe("PositionInfo") {
    describe("#showMessage") {
      it("should show an error message with the position which occured") {
        PositionInfo.showMessage(NoPosition, "test message") should be (
          "<undefined position>: test message"
        )
        PositionInfo.showMessage(OffsetPosition("test\nsource", 6), "test message") should be (
          "2.2: test message\n" +
          "source\n" +
          " ^"
        )
      }
    }
  }
}
