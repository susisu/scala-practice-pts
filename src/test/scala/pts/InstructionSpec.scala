package pts

import org.scalatest._

class InstructionSpec extends FunSpec with Matchers {
  implicit object UnitInfo extends SourceInfo[Unit] {
    type noInfoType = Unit
    def noInfo = ()
    def showMessage(info: Unit, msg: String): String = msg
  }

  describe("InAssume[I]") {
    describe("#exec") {
      it("should add an assumed variable to the environemnt") {
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
        );
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

  describe("InDefine[I]") {
    describe("#exec") {
      it("should add a term to the environment") {
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
        );
        {
          val term = TmAbs((), "x",
            TmVar((), "T"),
            TmVar((), "x")
          )
          val inst = InDefine((), "id", None, term)
          val res = inst.exec(pts, env)
          res.isRight should be (true)
          val (msg, _env) = res.right.get
          msg should include ("id")
          _env should equal (Map(
            "T" -> ((
              TmConst((), "*"),
              None
            )),
            "id" -> ((
              TmProd((), "x",
                TmVar((), "T"),
                TmVar((), "T")
              ),
              Some(
                TmAbs((), "x",
                  TmVar((), "T"),
                  TmVar((), "x")
                )
              )
            ))
          ))
        }
        {
          val term = TmAbs((), "x",
            TmVar((), "T"),
            TmVar((), "x")
          )
          val itsType = TmApp((),
            TmAbs((), "X",
              TmConst((), "*"),
              TmVar((), "X")
            ),
            TmProd((), "y",
              TmVar((), "T"),
              TmVar((), "T")
            )
          )
          val inst = InDefine((), "id", Some(itsType), term)
          val res = inst.exec(pts, env)
          res.isRight should be (true)
          val (msg, _env) = res.right.get
          msg should include ("id")
          _env should equal (Map(
            "T" -> ((
              TmConst((), "*"),
              None
            )),
            "id" -> ((
              TmApp((),
                TmAbs((), "X",
                  TmConst((), "*"),
                  TmVar((), "X")
                ),
                TmProd((), "y",
                  TmVar((), "T"),
                  TmVar((), "T")
                )
              ),
              Some(
                TmAbs((), "x",
                  TmVar((), "T"),
                  TmVar((), "x")
                )
              )
            ))
          ))
        }
        {
          val term = TmAbs((), "x",
            TmVar((), "T"),
            TmVar((), "x")
          )
          val inst = InDefine((), "T", None, term)
          val res = inst.exec(pts, env)
          res.isLeft should be (true)
          val msg = res.left.get
          msg should include ("already declared")
        }
        {
          val term = TmAbs((), "x",
            TmVar((), "T"),
            TmVar((), "y")
          )
          val inst = InDefine((), "id", None, term)
          val res = inst.exec(pts, env)
          res.isLeft should be (true)
          val msg = res.left.get
          msg should include ("not declared")
        }
        {
          val term = TmAbs((), "x",
            TmVar((), "T"),
            TmVar((), "x")
          )
          val itsType = TmVar((), "T")
          val inst = InDefine((), "id", Some(itsType), term)
          val res = inst.exec(pts, env)
          res.isLeft should be (true)
          val msg = res.left.get
          msg should include ("does not match")
        }
      }
    }
  }

  describe("InPrint[I]") {
    describe("#exec") {
      it("should print the value of the declared variable") {
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
          )),
          "Id" -> ((
            TmProd((), "X",
              TmConst((), "*"),
              TmConst((), "*")
            ),
            Some(
              TmAbs((), "X",
                TmConst((), "*"),
                TmVar((), "X")
              )
            )
          ))
        );
        {
          val inst = InPrint((), "T")
          val res = inst.exec(pts, env)
          res.isRight should be (true)
          val (msg, _env) = res.right.get
          msg should be ("T: *")
          _env should equal (Map(
            "T" -> ((
              TmConst((), "*"),
              None
            )),
            "Id" -> ((
              TmProd((), "X",
                TmConst((), "*"),
                TmConst((), "*")
              ),
              Some(
                TmAbs((), "X",
                  TmConst((), "*"),
                  TmVar((), "X")
                )
              )
            ))
          ))
        }
        {
          val inst = InPrint((), "Id")
          val res = inst.exec(pts, env)
          res.isRight should be (true)
          val (msg, _env) = res.right.get
          msg should be ("Id: * -> *\n= fun X: *. X")
          _env should equal (Map(
            "T" -> ((
              TmConst((), "*"),
              None
            )),
            "Id" -> ((
              TmProd((), "X",
                TmConst((), "*"),
                TmConst((), "*")
              ),
              Some(
                TmAbs((), "X",
                  TmConst((), "*"),
                  TmVar((), "X")
                )
              )
            ))
          ))
        }
        {
          val inst = InPrint((), "unknown")
          val res = inst.exec(pts, env)
          res.isLeft should be (true)
          res.left.get should include ("not declared")
        }
      }
    }
  }
}
