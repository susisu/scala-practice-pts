package pts

import scala.collection.immutable.Set
import org.scalatest._

class UtilSpec extends FunSpec with Matchers {
  describe("Util") {
    describe("#getFreshVarName(prefix: String, usedName: Set[String]): String") {
      it("should return a fresh name not contained in the specified used name set") {
        Util.getFreshVarName("x", Set.empty) should equal ("x0")
        Util.getFreshVarName("y", Set.empty) should equal ("y0")
        Util.getFreshVarName("x", Set("x0")) should equal ("x1")
        Util.getFreshVarName("x", Set("x0", "x1")) should equal ("x2")
      }
    }
  }
}
