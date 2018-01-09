package pts

import scala.collection.immutable.Set
import org.scalatest._

class UtilSpec extends FunSpec with Matchers {
  describe("Util") {
    describe("#chopDigits(str: String): String") {
      it("should chop the trailing digits in the string") {
        Util.chopDigits("test") should equal ("test")
        Util.chopDigits("test123") should equal ("test")
        Util.chopDigits("a123b456") should equal ("a123b")
      }
    }

    describe("#getFreshVarName(src: String, usedName: Set[String]): String") {
      it("should return a fresh name not contained in the specified used name set") {
        Util.getFreshVarName("x", Set.empty) should equal ("x0")
        Util.getFreshVarName("y", Set.empty) should equal ("y0")
        Util.getFreshVarName("x", Set("x0")) should equal ("x1")
        Util.getFreshVarName("x", Set("x0", "x1")) should equal ("x2")
        Util.getFreshVarName("x0", Set("x0", "x1")) should equal ("x2")
      }
    }
  }
}
