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
            TmConst(Some(()), "*"),
            None
          )),
          "U" -> ((
            TmConst(Some(()), "*"),
            None
          )),
          "t" -> ((
            TmVar(Some(()), "T"),
            None
          )),
          "u" -> ((
            TmVar(Some(()), "U"),
            None
          )),
          "f" -> ((
            TmProd(Some(()), "x",
              TmVar(Some(()), "T"),
              TmVar(Some(()), "U")
            ),
            None
          )),
          "F" -> ((
            TmProd(Some(()), "T",
              TmConst(Some(()), "*"),
              TmConst(Some(()), "*")
            ),
            None
          )),
          "K" -> ((
            TmProd(Some(()), "T",
              TmConst(Some(()), "*"),
              TmProd(Some(()), "U",
                TmConst(Some(()), "*"),
                TmConst(Some(()), "*")
              )
            ),
            Some(
              TmAbs(Some(()), "T",
                TmConst(Some(()), "*"),
                TmAbs(Some(()), "U",
                  TmConst(Some(()), "*"),
                  TmVar(Some(()), "T")
                )
              )
            )
          ))
        );
        {
          val term = TmVar(Some(()), "t")
          val res = pts.typeOf(env, term)
          res.isRight should be (true)
          res.right.get.alphaEquals(
            TmVar(Some(()), "T")
          ) should be (true)
        }
        {
          val term = TmVar(Some(()), "x")
          val res = pts.typeOf(env, term)
          res.isLeft should be (true)
          res.left.get should include ("not declared")
        }
        {
          val term = TmConst(Some(()), "*")
          val res = pts.typeOf(env, term)
          res.isRight should be (true)
          res.right.get.alphaEquals(
            TmConst(Some(()), "#")
          ) should be (true)
        }
        {
          val term = TmConst(Some(()), "#")
          val res = pts.typeOf(env, term)
          res.isLeft should be (true)
          res.left.get should include ("no axiom")
        }
        {
          val term = TmApp(Some(()),
            TmVar(Some(()), "f"),
            TmVar(Some(()), "t")
          )
          val res = pts.typeOf(env, term)
          res.isRight should be (true)
          res.right.get.alphaEquals(
            TmVar(Some(()), "U")
          ) should be (true)
        }
        {
          val term = TmApp(Some(()),
            TmVar(Some(()), "F"),
            TmApp(Some(()),
              TmApp(Some(()),
                TmVar(Some(()), "K"),
                TmVar(Some(()), "T")
              ),
              TmVar(Some(()), "U")
            )
          )
          val res = pts.typeOf(env, term)
          res.isRight should be (true)
          res.right.get.alphaEquals(
            TmConst(Some(()), "*")
          ) should be (true)
        }
        {
          val term = TmApp(Some(()),
            TmVar(Some(()), "t"),
            TmVar(Some(()), "t")
          )
          val res = pts.typeOf(env, term)
          res.isLeft should be (true)
          res.left.get should include ("function")
        }
        {
          val term = TmApp(Some(()),
            TmVar(Some(()), "f"),
            TmVar(Some(()), "u")
          )
          val res = pts.typeOf(env, term)
          res.isLeft should be (true)
          res.left.get should include ("argument")
        }
        {
          val term = TmAbs(Some(()), "x",
            TmVar(Some(()), "T"),
            TmVar(Some(()), "x")
          )
          val res = pts.typeOf(env, term)
          res.isRight should be (true)
          res.right.get.alphaEquals(
            TmProd(Some(()), "x",
              TmVar(Some(()), "T"),
              TmVar(Some(()), "T")
            )
          ) should be (true)
        }
        {
          val term = TmAbs(Some(()), "u",
            TmVar(Some(()), "T"),
            TmVar(Some(()), "u")
          )
          val res = pts.typeOf(env, term)
          res.isRight should be (true)
          res.right.get.alphaEquals(
            TmProd(Some(()), "u",
              TmVar(Some(()), "T"),
              TmVar(Some(()), "T")
            )
          ) should be (true)
        }
        {
          val term = TmAbs(Some(()), "x",
            TmConst(Some(()), "*"),
            TmVar(Some(()), "T")
          )
          val res = pts.typeOf(env, term)
          res.isRight should be (true)
          res.right.get.alphaEquals(
            TmProd(Some(()), "x",
              TmConst(Some(()), "*"),
              TmConst(Some(()), "*")
            )
          ) should be (true)
        }
        {
          val term = TmAbs(Some(()), "x",
            TmVar(Some(()), "T"),
            TmConst(Some(()), "*")
          )
          val res = pts.typeOf(env, term)
          res.isLeft should be (true)
          res.left.get should include ("no axiom")
        }
        {
          val term = TmAbs(Some(()), "x",
            TmConst(Some(()), "*"),
            TmConst(Some(()), "*")
          )
          val res = pts.typeOf(env, term)
          res.isLeft should be (true)
          res.left.get should include ("no axiom")
        }
        {
          val term = TmProd(Some(()), "x",
            TmVar(Some(()), "T"),
            TmVar(Some(()), "T")
          )
          val res = pts.typeOf(env, term)
          res.isRight should be (true)
          res.right.get.alphaEquals(
            TmConst(Some(()), "*")
          ) should be (true)
        }
        {
          val term = TmProd(Some(()), "u",
            TmVar(Some(()), "T"),
            TmVar(Some(()), "T")
          )
          val res = pts.typeOf(env, term)
          res.isRight should be (true)
          res.right.get.alphaEquals(
            TmConst(Some(()), "*")
          ) should be (true)
        }
        {
          val term = TmProd(Some(()), "x",
            TmVar(Some(()), "T"),
            TmConst(Some(()), "*")
          )
          val res = pts.typeOf(env, term)
          res.isRight should be (true)
          res.right.get.alphaEquals(
            TmConst(Some(()), "#")
          ) should be (true)
        }
        {
          val term = TmProd(Some(()), "x",
            TmConst(Some(()), "*"),
            TmConst(Some(()), "*")
          )
          val res = pts.typeOf(env, term)
          res.isRight should be (true)
          res.right.get.alphaEquals(
            TmConst(Some(()), "#")
          ) should be (true)
        }
        {
          val term = TmProd(Some(()), "x",
            TmConst(Some(()), "*"),
            TmVar(Some(()), "T")
          )
          val res = pts.typeOf(env, term)
          res.isLeft should be (true)
          res.left.get should include ("no rule")
        }
        {
          val term = TmProd(Some(()), "x",
            TmVar(Some(()), "t"),
            TmVar(Some(()), "T")
          )
          val res = pts.typeOf(env, term)
          res.isLeft should be (true)
          res.left.get should include ("not a sort")
        }
        {
          val term = TmProd(Some(()), "x",
            TmVar(Some(()), "T"),
            TmVar(Some(()), "x")
          )
          val res = pts.typeOf(env, term)
          res.isLeft should be (true)
          res.left.get should include ("not a sort")
        }
      }
    }
  }
}
