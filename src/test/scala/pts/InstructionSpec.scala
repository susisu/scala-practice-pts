package pts

import org.scalatest._

class InstructionSpec extends FunSpec with Matchers {
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

  val env = Map(
    "T" -> ((
      TmConst((), "*"),
      None
    ))
  )

  implicit object UnitInfo extends SourceInfo[Unit] {
    type noInfoType = Unit
    def noInfo = ()
    def showMessage(info: Unit, msg: String): String = msg
  }

  describe("InAssume[I]") {
    describe("#exec") {
      it("should add an assumed variable to the environemnt") {
        {
          val itsType = TmProd((), "A",
            TmConst((), "*"),
            TmVar((), "A")
          )
          val inst = InAssume((), "bottom", itsType)
          val res = inst.exec(pts, env)
          res.isRight should be (true)
          val (msg, _env) = res.right.get
          msg should include ("bottom")
          _env should equal (Map(
            "T" -> ((
              TmConst((), "*"),
              None
            )),
            "bottom" -> ((
              TmProd((), "A",
                TmConst((), "*"),
                TmVar((), "A")
              ),
              None
            ))
          ))
        }
        {
          val itsType = TmProd((), "A",
            TmConst((), "*"),
            TmVar((), "A")
          )
          val inst = InAssume((), "T", itsType)
          val res = inst.exec(pts, env)
          res.isLeft should be (true)
          val msg = res.left.get
          msg should include ("already declared")
        }
        {
          val itsType = TmProd((), "A",
            TmConst((), "*"),
            TmVar((), "B")
          )
          val inst = InAssume((), "error", itsType)
          val res = inst.exec(pts, env)
          res.isLeft should be (true)
          val msg = res.left.get
          msg should include ("not declared")
        }
        {
          val itsType = TmAbs((), "x",
            TmVar((), "T"),
            TmVar((), "x")
          )
          val inst = InAssume((), "id", itsType)
          val res = inst.exec(pts, env)
          res.isLeft should be (true)
          val msg = res.left.get
          msg should include ("not a type")
        }
      }
    }
  }
}
