package pts

import scala.collection.immutable.Set

object Util {
  def getFreshVarName(prefix: String, usedNames: Set[String]): String = {
    var n = 0
    var name = prefix + n.toString
    while (usedNames.contains(name)) {
      n += 1
      name = prefix + n.toString
    }
    name
  }
}
