package pts

import scala.collection.immutable.Set
import org.scalatest._

class PTSSpec extends FunSpec with Matchers {
  implicit object UnitInfo extends SourceInfo[Unit] {
    type noInfoType = Unit
    def noInfo = ()
    def showMessage(info: Unit, msg: String): String = msg
  }

  describe("PTS") {
    describe("#typeOf[I](env: Term.Env[Option[I]], term: Term[Option[I]]): Term[Option[I]]") {
      it("should compute the type of the given term") {
        val pts = PTS(
          Set("*", "#"),
          Map("*" -> "#"),
          Map(
            ("*", "*") -> "*",
            ("*", "#") -> "#",
            ("#", "#") -> "#"
          )
        )
        val env = Map(
          "T" -> ((
            TmConst((), "*"),
            None
          )),
          "U" -> ((
            TmConst((), "*"),
            None
          )),
          "t" -> ((
            TmVar((), "T"),
            None
          )),
          "u" -> ((
            TmVar((), "U"),
            None
          )),
          "f" -> ((
            TmProd((), "x",
              TmVar((), "T"),
              TmVar((), "U")
            ),
            None
          )),
          "F" -> ((
            TmProd((), "T",
              TmConst((), "*"),
              TmConst((), "*")
            ),
            None
          )),
          "K" -> ((
            TmProd((), "T",
              TmConst((), "*"),
              TmProd((), "U",
                TmConst((), "*"),
                TmConst((), "*")
              )
            ),
            Some(
              TmAbs((), "T",
                TmConst((), "*"),
                TmAbs((), "U",
                  TmConst((), "*"),
                  TmVar((), "T")
                )
              )
            )
          ))
        );
        {
          val term = TmVar((), "t")
          val res = pts.typeOf(env, term)
          res.isRight should be (true)
          res.right.get.alphaEquals(
            TmVar((), "T")
          ) should be (true)
        }
        {
          val term = TmVar((), "x")
          val res = pts.typeOf(env, term)
          res.isLeft should be (true)
          res.left.get should include ("not declared")
        }
        {
          val term = TmConst((), "*")
          val res = pts.typeOf(env, term)
          res.isRight should be (true)
          res.right.get.alphaEquals(
            TmConst((), "#")
          ) should be (true)
        }
        {
          val term = TmConst((), "#")
          val res = pts.typeOf(env, term)
          res.isLeft should be (true)
          res.left.get should include ("no axiom")
        }
        {
          val term = TmApp((),
            TmVar((), "f"),
            TmVar((), "t")
          )
          val res = pts.typeOf(env, term)
          res.isRight should be (true)
          res.right.get.alphaEquals(
            TmVar((), "U")
          ) should be (true)
        }
        {
          val term = TmApp((),
            TmVar((), "F"),
            TmApp((),
              TmApp((),
                TmVar((), "K"),
                TmVar((), "T")
              ),
              TmVar((), "U")
            )
          )
          val res = pts.typeOf(env, term)
          res.isRight should be (true)
          res.right.get.alphaEquals(
            TmConst((), "*")
          ) should be (true)
        }
        {
          val term = TmApp((),
            TmVar((), "t"),
            TmVar((), "t")
          )
          val res = pts.typeOf(env, term)
          res.isLeft should be (true)
          res.left.get should include ("function")
        }
        {
          val term = TmApp((),
            TmVar((), "f"),
            TmVar((), "u")
          )
          val res = pts.typeOf(env, term)
          res.isLeft should be (true)
          res.left.get should include ("argument")
        }
        {
          val term = TmAbs((), "x",
            TmVar((), "T"),
            TmVar((), "x")
          )
          val res = pts.typeOf(env, term)
          res.isRight should be (true)
          res.right.get.alphaEquals(
            TmProd((), "x",
              TmVar((), "T"),
              TmVar((), "T")
            )
          ) should be (true)
        }
        {
          val term = TmAbs((), "u",
            TmVar((), "T"),
            TmVar((), "u")
          )
          val res = pts.typeOf(env, term)
          res.isRight should be (true)
          res.right.get.alphaEquals(
            TmProd((), "u",
              TmVar((), "T"),
              TmVar((), "T")
            )
          ) should be (true)
        }
        {
          val term = TmAbs((), "x",
            TmConst((), "*"),
            TmVar((), "T")
          )
          val res = pts.typeOf(env, term)
          res.isRight should be (true)
          res.right.get.alphaEquals(
            TmProd((), "x",
              TmConst((), "*"),
              TmConst((), "*")
            )
          ) should be (true)
        }
        {
          val term = TmAbs((), "x",
            TmVar((), "T"),
            TmConst((), "*")
          )
          val res = pts.typeOf(env, term)
          res.isLeft should be (true)
          res.left.get should include ("no axiom")
        }
        {
          val term = TmAbs((), "x",
            TmConst((), "*"),
            TmConst((), "*")
          )
          val res = pts.typeOf(env, term)
          res.isLeft should be (true)
          res.left.get should include ("no axiom")
        }
        {
          val term = TmProd((), "x",
            TmVar((), "T"),
            TmVar((), "T")
          )
          val res = pts.typeOf(env, term)
          res.isRight should be (true)
          res.right.get.alphaEquals(
            TmConst((), "*")
          ) should be (true)
        }
        {
          val term = TmProd((), "u",
            TmVar((), "T"),
            TmVar((), "T")
          )
          val res = pts.typeOf(env, term)
          res.isRight should be (true)
          res.right.get.alphaEquals(
            TmConst((), "*")
          ) should be (true)
        }
        {
          val term = TmProd((), "x",
            TmVar((), "T"),
            TmConst((), "*")
          )
          val res = pts.typeOf(env, term)
          res.isRight should be (true)
          res.right.get.alphaEquals(
            TmConst((), "#")
          ) should be (true)
        }
        {
          val term = TmProd((), "x",
            TmConst((), "*"),
            TmConst((), "*")
          )
          val res = pts.typeOf(env, term)
          res.isRight should be (true)
          res.right.get.alphaEquals(
            TmConst((), "#")
          ) should be (true)
        }
        {
          val term = TmProd((), "x",
            TmConst((), "*"),
            TmVar((), "T")
          )
          val res = pts.typeOf(env, term)
          res.isLeft should be (true)
          res.left.get should include ("no rule")
        }
        {
          val term = TmProd((), "x",
            TmVar((), "t"),
            TmVar((), "T")
          )
          val res = pts.typeOf(env, term)
          res.isLeft should be (true)
          res.left.get should include ("not a sort")
        }
        {
          val term = TmProd((), "x",
            TmVar((), "T"),
            TmVar((), "x")
          )
          val res = pts.typeOf(env, term)
          res.isLeft should be (true)
          res.left.get should include ("not a sort")
        }
      }
    }
  }
}
