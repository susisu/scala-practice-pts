package pts

import scala.io._
import scala.util.parsing.input._

object Main {
  implicit val si = PositionInfo

  def repl(pts: PTS, env: Term.Env[Position], buffer: String, prompt: String): Unit = {
    val line = StdIn.readLine(prompt)
    if (line == null) {
      // end of input
      println()
      repl(pts, env, "", "> ")
    }
    else {
      val _buffer = buffer + line + "\n"
      if (_buffer.length > 0 && _buffer.charAt(0) == ':') {
        // command
        val command = _buffer.substring(1).trim
        command match {
          case "q" | "e" | "quit" | "exit" => ()
          case _ => {
            println(s"unknown command `:$command`")
            repl(pts, env, "", "> ")
          }
        }
      }
      else {
        // instruction
        val res = Parser.parse(Parser.instructions, _buffer)
        if (res.successful) {
          for (inst <- res.get) {
            inst.exec(pts, env) match {
              case Left(err) => {
                println(err)
                repl(pts, env, "", "> ")
              }
              case Right((msg, _env)) => {
                println(msg)
                repl(pts, _env, "", "> ")
              }
            }
          }
        }
        else {
          repl(pts, env, _buffer, "| ")
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val pts = PTS(
      Set("*", "#"),
      Map("*" -> "#"),
      Map(
        ("*", "*") -> "*",
        ("*", "#") -> "#",
        ("#", "*") -> "*",
        ("#", "#") -> "#"
      )
    )
    val env: Term.Env[Position] = Map()
    repl(pts, env, "", "> ")
  }
}
