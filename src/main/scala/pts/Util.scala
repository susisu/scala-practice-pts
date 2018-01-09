package pts

import scala.collection.immutable.Set

object Util {
  def chopDigits(str: String): String = "[0-9]*$".r.replaceFirstIn(str, "")

  def getFreshVarName(src: String, usedNames: Set[String]): String = {
    val prefix = chopDigits(src)
    var n = 0
    var name = prefix + n.toString
    while (usedNames.contains(name)) {
      n += 1
      name = prefix + n.toString
    }
    name
  }
}
