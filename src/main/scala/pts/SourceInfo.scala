package pts

import scala.util.parsing.input._

trait SourceInfo[I] {
  type noInfoType <: I
  def noInfo: noInfoType
  def showMessage(info: I, msg: String): String
}

object PositionInfo extends SourceInfo[Position] {
  type noInfoType = NoPosition.type
  def noInfo = NoPosition
  def showMessage(info: Position, msg: String): String =
    info match {
      case _: NoPosition.type => s"${info.toString}: $msg"
      case _ => s"${info.toString}: $msg\n${info.longString}"
    }
}
