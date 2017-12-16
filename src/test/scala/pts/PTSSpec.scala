package pts

import scala.collection.immutable.Set
import org.scalatest._

class PTSSpec extends FunSpec with Matchers {
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
            TmConst(Some(Unit), "*"),
            None
          )),
          "U" -> ((
            TmConst(Some(Unit), "*"),
            None
          )),
          "t" -> ((
            TmVar(Some(Unit), "T"),
            None
          )),
          "u" -> ((
            TmVar(Some(Unit), "U"),
            None
          )),
          "f" -> ((
            TmProd(Some(Unit), "x",
              TmVar(Some(Unit), "T"),
              TmVar(Some(Unit), "U")
            ),
            None
          )),
          "F" -> ((
            TmProd(Some(Unit), "T",
              TmConst(Some(Unit), "*"),
              TmConst(Some(Unit), "*")
            ),
            None
          )),
          "K" -> ((
            TmProd(Some(Unit), "T",
              TmConst(Some(Unit), "*"),
              TmProd(Some(Unit), "U",
                TmConst(Some(Unit), "*"),
                TmConst(Some(Unit), "*")
              )
            ),
            Some(
              TmAbs(Some(Unit), "T",
                TmConst(Some(Unit), "*"),
                TmAbs(Some(Unit), "U",
                  TmConst(Some(Unit), "*"),
                  TmVar(Some(Unit), "T")
                )
              )
            )
          ))
        );
        {
          val term = TmVar(Some(Unit), "t")
          val res = pts.typeOf(env, term)
          res.isRight should be (true)
          res.right.get.alphaEquals(
            TmVar(Some(Unit), "T")
          ) should be (true)
        }
        {
          val term = TmVar(Some(Unit), "x")
          val res = pts.typeOf(env, term)
          res.isLeft should be (true)
          res.left.get should include ("not declared")
        }
        {
          val term = TmConst(Some(Unit), "*")
          val res = pts.typeOf(env, term)
          res.isRight should be (true)
          res.right.get.alphaEquals(
            TmConst(Some(Unit), "#")
          ) should be (true)
        }
        {
          val term = TmConst(Some(Unit), "#")
          val res = pts.typeOf(env, term)
          res.isLeft should be (true)
          res.left.get should include ("no axiom")
        }
        {
          val term = TmApp(Some(Unit),
            TmVar(Some(Unit), "f"),
            TmVar(Some(Unit), "t")
          )
          val res = pts.typeOf(env, term)
          res.isRight should be (true)
          res.right.get.alphaEquals(
            TmVar(Some(Unit), "U")
          ) should be (true)
        }
        {
          val term = TmApp(Some(Unit),
            TmVar(Some(Unit), "F"),
            TmApp(Some(Unit),
              TmApp(Some(Unit),
                TmVar(Some(Unit), "K"),
                TmVar(Some(Unit), "T")
              ),
              TmVar(Some(Unit), "U")
            )
          )
          val res = pts.typeOf(env, term)
          res.isRight should be (true)
          res.right.get.alphaEquals(
            TmConst(Some(Unit), "*")
          ) should be (true)
        }
        {
          val term = TmApp(Some(Unit),
            TmVar(Some(Unit), "t"),
            TmVar(Some(Unit), "t")
          )
          val res = pts.typeOf(env, term)
          res.isLeft should be (true)
          res.left.get should include ("function")
        }
        {
          val term = TmApp(Some(Unit),
            TmVar(Some(Unit), "f"),
            TmVar(Some(Unit), "u")
          )
          val res = pts.typeOf(env, term)
          res.isLeft should be (true)
          res.left.get should include ("argument")
        }
        {
          val term = TmAbs(Some(Unit), "x",
            TmVar(Some(Unit), "T"),
            TmVar(Some(Unit), "x")
          )
          val res = pts.typeOf(env, term)
          res.isRight should be (true)
          res.right.get.alphaEquals(
            TmProd(Some(Unit), "x",
              TmVar(Some(Unit), "T"),
              TmVar(Some(Unit), "T")
            )
          ) should be (true)
        }
        {
          val term = TmAbs(Some(Unit), "u",
            TmVar(Some(Unit), "T"),
            TmVar(Some(Unit), "u")
          )
          val res = pts.typeOf(env, term)
          res.isRight should be (true)
          res.right.get.alphaEquals(
            TmProd(Some(Unit), "u",
              TmVar(Some(Unit), "T"),
              TmVar(Some(Unit), "T")
            )
          ) should be (true)
        }
        {
          val term = TmAbs(Some(Unit), "x",
            TmConst(Some(Unit), "*"),
            TmVar(Some(Unit), "T")
          )
          val res = pts.typeOf(env, term)
          res.isRight should be (true)
          res.right.get.alphaEquals(
            TmProd(Some(Unit), "x",
              TmConst(Some(Unit), "*"),
              TmConst(Some(Unit), "*")
            )
          ) should be (true)
        }
        {
          val term = TmAbs(Some(Unit), "x",
            TmVar(Some(Unit), "T"),
            TmConst(Some(Unit), "*")
          )
          val res = pts.typeOf(env, term)
          res.isLeft should be (true)
          res.left.get should include ("no axiom")
        }
        {
          val term = TmAbs(Some(Unit), "x",
            TmConst(Some(Unit), "*"),
            TmConst(Some(Unit), "*")
          )
          val res = pts.typeOf(env, term)
          res.isLeft should be (true)
          res.left.get should include ("no axiom")
        }
        {
          val term = TmProd(Some(Unit), "x",
            TmVar(Some(Unit), "T"),
            TmVar(Some(Unit), "T")
          )
          val res = pts.typeOf(env, term)
          res.isRight should be (true)
          res.right.get.alphaEquals(
            TmConst(Some(Unit), "*")
          ) should be (true)
        }
        {
          val term = TmProd(Some(Unit), "u",
            TmVar(Some(Unit), "T"),
            TmVar(Some(Unit), "T")
          )
          val res = pts.typeOf(env, term)
          res.isRight should be (true)
          res.right.get.alphaEquals(
            TmConst(Some(Unit), "*")
          ) should be (true)
        }
        {
          val term = TmProd(Some(Unit), "x",
            TmVar(Some(Unit), "T"),
            TmConst(Some(Unit), "*")
          )
          val res = pts.typeOf(env, term)
          res.isRight should be (true)
          res.right.get.alphaEquals(
            TmConst(Some(Unit), "#")
          ) should be (true)
        }
        {
          val term = TmProd(Some(Unit), "x",
            TmConst(Some(Unit), "*"),
            TmConst(Some(Unit), "*")
          )
          val res = pts.typeOf(env, term)
          res.isRight should be (true)
          res.right.get.alphaEquals(
            TmConst(Some(Unit), "#")
          ) should be (true)
        }
        {
          val term = TmProd(Some(Unit), "x",
            TmConst(Some(Unit), "*"),
            TmVar(Some(Unit), "T")
          )
          val res = pts.typeOf(env, term)
          res.isLeft should be (true)
          res.left.get should include ("no rule")
        }
        {
          val term = TmProd(Some(Unit), "x",
            TmVar(Some(Unit), "t"),
            TmVar(Some(Unit), "T")
          )
          val res = pts.typeOf(env, term)
          res.isLeft should be (true)
          res.left.get should include ("not a sort")
        }
        {
          val term = TmProd(Some(Unit), "x",
            TmVar(Some(Unit), "T"),
            TmVar(Some(Unit), "x")
          )
          val res = pts.typeOf(env, term)
          res.isLeft should be (true)
          res.left.get should include ("not a sort")
        }
      }
    }
  }
}
