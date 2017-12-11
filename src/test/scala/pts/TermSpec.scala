package pts

import scala.collection.immutable._
import org.scalatest._

class TermSpec extends FunSpec with Matchers {
  describe("TmVar[I]") {
    describe("#freeVars: Set[String]") {
      it("should be a singleton set which contains its name") {
        val term = TmVar(Unit, "x")
        term.freeVars should equal (Set("x"))
      }
    }

    describe("#toString(): String") {
      it("should return its name") {
        val term = TmVar(Unit, "x")
        term.toString should be ("x")
      }
    }

    describe("#renameFreeVar(oldName: String, newName: String): TmVar[I]") {
      it("should rename its name if it is the same as the specified old name") {
        val term = TmVar(Unit, "x")
        term.renameFreeVar("x", "z") should equal (TmVar(Unit, "z"))
        term.renameFreeVar("y", "z") should equal (TmVar(Unit, "x"))
      }
    }

    describe("#alphaEquals(term: Term[I]): Boolean") {
      it("should return true if the given term is a variable of the same name") {
        val term = TmVar(Unit, "x")
        term.alphaEquals(TmVar(Unit, "x")) should be (true)
        term.alphaEquals(TmVar(Unit, "y")) should be (false)
        term.alphaEquals(TmConst(Unit, "x")) should be (false)
        term.alphaEquals(TmApp(Unit, TmVar(Unit, "f"), TmVar(Unit, "x"))) should be (false)
        term.alphaEquals(TmAbs(Unit, "x", TmVar(Unit, "T"), TmVar(Unit, "x"))) should be (false)
        term.alphaEquals(TmProd(Unit, "x", TmVar(Unit, "T"), TmVar(Unit, "x"))) should be (false)
      }
    }

    describe("substitute(name: String, term: Term[I]): Term[I]") {
      it("should return the given term if and only if the given name is the same as the variable name") {
        val term = TmVar(Unit, "x")
        term.substitute("x", TmVar(Unit, "y")) should equal (TmVar(Unit, "y"))
        term.substitute("y", TmVar(Unit, "z")) should equal (TmVar(Unit, "x"))
      }
    }
  }

  describe("TmConst[I]") {
    describe("#freeVars: Set[String]") {
      it("should be an empty set") {
        val term = TmConst(Unit, "*")
        term.freeVars should equal (Set.empty)
      }
    }

    describe("#toString(): String") {
      it("should return its name") {
        val term = TmConst(Unit, "*")
        term.toString should be ("*")
      }
    }

    describe("#renameFreeVar(oldName: String, newName: String): TmConst[I]") {
      it("should always return itself") {
        val term = TmConst(Unit, "*")
        term.renameFreeVar("x", "z") should equal (TmConst(Unit, "*"))
        term.renameFreeVar("*", "#") should equal (TmConst(Unit, "*"))
      }
    }

    describe("#alphaEquals(term: Term[I]): Boolean") {
      it("should return true if the given term is a constant of the same name") {
        val term = TmConst(Unit, "*")
        term.alphaEquals(TmConst(Unit, "*")) should be (true)
        term.alphaEquals(TmConst(Unit, "#")) should be (false)
        term.alphaEquals(TmVar(Unit, "*")) should be (false)
        term.alphaEquals(TmApp(Unit, TmVar(Unit, "f"), TmVar(Unit, "x"))) should be (false)
        term.alphaEquals(TmAbs(Unit, "x", TmVar(Unit, "T"), TmVar(Unit, "x"))) should be (false)
        term.alphaEquals(TmProd(Unit, "x", TmVar(Unit, "T"), TmVar(Unit, "x"))) should be (false)
      }
    }

    describe("substitute(name: String, term: Term[I]): Term[I]") {
      it("should always return itself") {
        val term = TmConst(Unit, "*")
        term.substitute("x", TmVar(Unit, "y")) should equal (TmConst(Unit, "*"))
        term.substitute("*", TmVar(Unit, "x")) should equal (TmConst(Unit, "*"))
      }
    }
  }

  describe("TmApp[I]") {
    describe("#freeVars: Set[String]") {
      it("should be an union set of the free variables of its function and argument") {
        val term = TmApp(Unit,
          TmVar(Unit, "f"),
          TmVar(Unit, "x")
        )
        term.freeVars should equal (Set("f", "x"))
      }
    }

    describe("#toString(): String") {
      it("should return a string representation of the application") {
        {
          val term = TmApp(Unit,
            TmVar(Unit, "f"),
            TmVar(Unit, "x")
          )
          term.toString should be ("f x")
        }
        {
          val term = TmApp(Unit,
            TmConst(Unit, "*"),
            TmVar(Unit, "x")
          )
          term.toString should be ("* x")
        }
        {
          val term = TmApp(Unit,
            TmApp(Unit,
              TmVar(Unit, "f"),
              TmVar(Unit, "x")
            ),
            TmVar(Unit, "y")
          )
          term.toString should be ("f x y")
        }
        {
          val term = TmApp(Unit,
            TmAbs(Unit, "x",
              TmVar(Unit, "T"),
              TmVar(Unit, "x")
            ),
            TmVar(Unit, "y")
          )
          term.toString should be ("(fun x: T. x) y")
        }
        {
          val term = TmApp(Unit,
            TmProd(Unit, "T",
              TmConst(Unit, "*"),
              TmVar(Unit, "T")
            ),
            TmVar(Unit, "U")
          )
          term.toString should be ("(forall T: *. T) U")
        }
        {
          val term = TmApp(Unit,
            TmVar(Unit, "f"),
            TmConst(Unit, "*")
          )
          term.toString should be ("f *")
        }
        {
          val term = TmApp(Unit,
            TmVar(Unit, "f"),
            TmApp(Unit,
              TmVar(Unit, "x"),
              TmVar(Unit, "y")
            )
          )
          term.toString should be ("f (x y)")
        }
        {
          val term = TmApp(Unit,
            TmVar(Unit, "f"),
            TmAbs(Unit, "x",
              TmVar(Unit, "T"),
              TmVar(Unit, "x")
            )
          )
          term.toString should be ("f (fun x: T. x)")
        }
        {
          val term = TmApp(Unit,
            TmVar(Unit, "f"),
            TmProd(Unit, "T",
              TmConst(Unit, "*"),
              TmVar(Unit, "T")
            )
          )
          term.toString should be ("f (forall T: *. T)")
        }
      }
    }

    describe("#renameFreeVar(oldName: String, newName: String): TmApp[I]") {
      it("should rename free variables in the function and the argument") {
        {
          val term = TmApp(Unit,
            TmVar(Unit, "f"),
            TmVar(Unit, "x")
          )
          term.renameFreeVar("f", "g") should equal (
            TmApp(Unit,
              TmVar(Unit, "g"),
              TmVar(Unit, "x")
            )
          )
          term.renameFreeVar("x", "z") should equal (
            TmApp(Unit,
              TmVar(Unit, "f"),
              TmVar(Unit, "z")
            )
          )
          term.renameFreeVar("y", "z") should equal (
            TmApp(Unit,
              TmVar(Unit, "f"),
              TmVar(Unit, "x")
            )
          )
        }
        {
          val term = TmApp(Unit,
            TmVar(Unit, "x"),
            TmVar(Unit, "x")
          )
          term.renameFreeVar("x", "z") should equal (
            TmApp(Unit,
              TmVar(Unit, "z"),
              TmVar(Unit, "z")
            )
          )
        }
      }
    }

    describe("#alphaEquals(term: Term[I]): Boolean") {
      it("should return true if the given term is an application of the equal function and argument") {
        val term = TmApp(Unit, TmVar(Unit, "f"), TmVar(Unit, "x"))
        term.alphaEquals(TmApp(Unit, TmVar(Unit, "f"), TmVar(Unit, "x"))) should be (true)
        term.alphaEquals(TmApp(Unit, TmVar(Unit, "g"), TmVar(Unit, "x"))) should be (false)
        term.alphaEquals(TmApp(Unit, TmVar(Unit, "f"), TmVar(Unit, "y"))) should be (false)
        term.alphaEquals(TmVar(Unit, "x")) should be (false)
        term.alphaEquals(TmConst(Unit, "*")) should be (false)
        term.alphaEquals(TmAbs(Unit, "x", TmVar(Unit, "T"), TmVar(Unit, "x"))) should be (false)
        term.alphaEquals(TmProd(Unit, "x", TmVar(Unit, "T"), TmVar(Unit, "x"))) should be (false)
      }
    }

    describe("substitute(name: String, term: Term[I]): Term[I]") {
      it("should return an application with variables in its function and argument substited") {
        {
          val term = TmApp(Unit,
            TmVar(Unit, "f"),
            TmVar(Unit, "x")
          )
          term.substitute("f", TmVar(Unit, "g")) should equal (
            TmApp(Unit,
              TmVar(Unit, "g"),
              TmVar(Unit, "x")
            )
          )
          term.substitute("x", TmVar(Unit, "y")) should equal (
            TmApp(Unit,
              TmVar(Unit, "f"),
              TmVar(Unit, "y")
            )
          )
          term.substitute("y", TmVar(Unit, "z")) should equal (
            TmApp(Unit,
              TmVar(Unit, "f"),
              TmVar(Unit, "x")
            )
          )
        }
        {
          val term = TmApp(Unit,
            TmVar(Unit, "x"),
            TmVar(Unit, "x")
          )
          term.substitute("x", TmVar(Unit, "y")) should equal (
            TmApp(Unit,
              TmVar(Unit, "y"),
              TmVar(Unit, "y")
            )
          )
        }
      }
    }
  }

  describe("TmAbs[I]") {
    describe("#freeVars: Set[String]") {
      it("should be a set of its free variables") {
        {
          val term = new TmAbs(Unit, "x",
            TmVar(Unit, "T"),
            TmApp(Unit,
              TmVar(Unit, "f"),
              TmVar(Unit, "x")
            )
          )
          term.freeVars should equal (Set("T", "f"))
        }
        {
          val term = new TmAbs(Unit, "x",
            TmVar(Unit, "x"),
            TmApp(Unit,
              TmVar(Unit, "f"),
              TmVar(Unit, "x")
            )
          )
          term.freeVars should equal (Set("x", "f"))
        }
      }
    }

    describe("#toString(): String") {
      it("should return a string representation of the abstraction") {
        val term = new TmAbs(Unit, "x",
          TmVar(Unit, "T"),
          TmVar(Unit, "x")
        )
        term.toString should be ("fun x: T. x")
      }
    }

    describe("#renameFreeVar(oldName: String, newName: String): TmAbs[I]") {
      it("should rename free variables in the parameter type and the body") {
        val term = new TmAbs(Unit, "x",
          TmVar(Unit, "T"),
          TmApp(Unit,
            TmVar(Unit, "f"),
            TmVar(Unit, "x")
          )
        )
        term.renameFreeVar("T", "U") should equal (
          new TmAbs(Unit, "x",
            TmVar(Unit, "U"),
            TmApp(Unit,
              TmVar(Unit, "f"),
              TmVar(Unit, "x")
            )
          )
        )
        term.renameFreeVar("f", "g") should equal (
          new TmAbs(Unit, "x",
            TmVar(Unit, "T"),
            TmApp(Unit,
              TmVar(Unit, "g"),
              TmVar(Unit, "x")
            )
          )
        )
        term.renameFreeVar("x", "y") should equal (
          new TmAbs(Unit, "x",
            TmVar(Unit, "T"),
            TmApp(Unit,
              TmVar(Unit, "f"),
              TmVar(Unit, "x")
            )
          )
        )
        term.renameFreeVar("y", "z") should equal (
          new TmAbs(Unit, "x",
            TmVar(Unit, "T"),
            TmApp(Unit,
              TmVar(Unit, "f"),
              TmVar(Unit, "x")
            )
          )
        )
      }
    }

    describe("#alphaEquals(term: Term[I]): Boolean") {
      it("should return true if the given term is an abstraction with the equal parameter type and body") {
        val term = TmAbs(Unit, "x", TmVar(Unit, "T"), TmVar(Unit, "x"))
        term.alphaEquals(TmAbs(Unit, "x", TmVar(Unit, "T"), TmVar(Unit, "x"))) should be (true)
        term.alphaEquals(TmAbs(Unit, "z", TmVar(Unit, "T"), TmVar(Unit, "z"))) should be (true)
        term.alphaEquals(TmAbs(Unit, "x", TmVar(Unit, "U"), TmVar(Unit, "x"))) should be (false)
        term.alphaEquals(TmAbs(Unit, "x", TmVar(Unit, "T"), TmVar(Unit, "y"))) should be (false)
        term.alphaEquals(TmVar(Unit, "x")) should be (false)
        term.alphaEquals(TmConst(Unit, "*")) should be (false)
        term.alphaEquals(TmApp(Unit, TmVar(Unit, "f"), TmVar(Unit, "x"))) should be (false)
        term.alphaEquals(TmProd(Unit, "x", TmVar(Unit, "T"), TmVar(Unit, "x"))) should be (false)
      }
    }

    describe("substitute(name: String, term: Term[I]): Term[I]") {
      it("should return an abstraction with variables in its parameter type and body substited") {
        {
          val term = TmAbs(Unit, "x",
            TmVar(Unit, "T"),
            TmVar(Unit, "x")
          )
          term.substitute("T", TmVar(Unit, "U")) should equal (
            TmAbs(Unit, "x",
              TmVar(Unit, "U"),
              TmVar(Unit, "x")
            )
          )
          term.substitute("x", TmVar(Unit, "y")) should equal (
            TmAbs(Unit, "x",
              TmVar(Unit, "T"),
              TmVar(Unit, "x")
            )
          )
        }
        {
          val term = TmAbs(Unit, "x",
            TmVar(Unit, "T"),
            TmApp(Unit,
              TmVar(Unit, "f"),
              TmVar(Unit, "x")
            )
          )
          term.substitute("f", TmVar(Unit, "g")) should equal (
            TmAbs(Unit, "x",
              TmVar(Unit, "T"),
              TmApp(Unit,
                TmVar(Unit, "g"),
                TmVar(Unit, "x")
              )
            )
          )
          term.substitute("f", TmVar(Unit, "x")) should equal (
            TmAbs(Unit, "_0",
              TmVar(Unit, "T"),
              TmApp(Unit,
                TmVar(Unit, "x"),
                TmVar(Unit, "_0")
              )
            )
          )
        }
      }
    }
  }

  describe("TmProd[I]") {
    describe("#freeVars: Set[String]") {
      it("should be a set of its free variables") {
        {
          val term = new TmProd(Unit, "x",
            TmVar(Unit, "T"),
            TmApp(Unit,
              TmVar(Unit, "f"),
              TmVar(Unit, "x")
            )
          )
          term.freeVars should equal (Set("T", "f"))
        }
        {
          val term = new TmProd(Unit, "x",
            TmVar(Unit, "x"),
            TmApp(Unit,
              TmVar(Unit, "f"),
              TmVar(Unit, "x")
            )
          )
          term.freeVars should equal (Set("x", "f"))
        }
      }
    }

    describe("#toString(): String") {
      it("should return a string representation of the product") {
        {
          val term = new TmProd(Unit, "_",
            TmVar(Unit, "T"),
            TmVar(Unit, "U")
          )
          term.toString should be ("T -> U")
        }
        {
          val term = new TmProd(Unit, "_",
            TmConst(Unit, "*"),
            TmVar(Unit, "T")
          )
          term.toString should be ("* -> T")
        }
        {
          val term = new TmProd(Unit, "_",
            TmApp(Unit,
              TmVar(Unit, "F"),
              TmVar(Unit, "T")
            ),
            TmVar(Unit, "U")
          )
          term.toString should be ("F T -> U")
        }
        {
          val term = new TmProd(Unit, "_",
            TmAbs(Unit, "T",
              TmConst(Unit, "*"),
              TmVar(Unit, "T")
            ),
            TmVar(Unit, "U")
          )
          term.toString should be ("(fun T: *. T) -> U")
        }
        {
          val term = new TmProd(Unit, "_",
            TmProd(Unit, "T",
              TmConst(Unit, "*"),
              TmVar(Unit, "T")
            ),
            TmVar(Unit, "U")
          )
          term.toString should be ("(forall T: *. T) -> U")
        }
        {
          val term = new TmProd(Unit, "_",
            TmVar(Unit, "T"),
            TmAbs(Unit, "U",
              TmConst(Unit, "*"),
              TmVar(Unit, "U")
            )
          )
          term.toString should be ("T -> fun U: *. U")
        }
        {
          val term = new TmProd(Unit, "_",
            TmVar(Unit, "T"),
            TmProd(Unit, "U",
              TmConst(Unit, "*"),
              TmVar(Unit, "U")
            )
          )
          term.toString should be ("T -> forall U: *. U")
        }
        {
          val term = new TmProd(Unit, "T",
            TmConst(Unit, "*"),
            TmVar(Unit, "T")
          )
          term.toString should be ("forall T: *. T")
        }
      }
    }

    describe("#renameFreeVar(oldName: String, newName: String): TmProd[I]") {
      it("should rename free variables in the parameter type and the body") {
        val term = new TmProd(Unit, "x",
          TmVar(Unit, "T"),
          TmApp(Unit,
            TmVar(Unit, "f"),
            TmVar(Unit, "x")
          )
        )
        term.renameFreeVar("T", "U") should equal (
          new TmProd(Unit, "x",
            TmVar(Unit, "U"),
            TmApp(Unit,
              TmVar(Unit, "f"),
              TmVar(Unit, "x")
            )
          )
        )
        term.renameFreeVar("f", "g") should equal (
          new TmProd(Unit, "x",
            TmVar(Unit, "T"),
            TmApp(Unit,
              TmVar(Unit, "g"),
              TmVar(Unit, "x")
            )
          )
        )
        term.renameFreeVar("x", "y") should equal (
          new TmProd(Unit, "x",
            TmVar(Unit, "T"),
            TmApp(Unit,
              TmVar(Unit, "f"),
              TmVar(Unit, "x")
            )
          )
        )
        term.renameFreeVar("y", "z") should equal (
          new TmProd(Unit, "x",
            TmVar(Unit, "T"),
            TmApp(Unit,
              TmVar(Unit, "f"),
              TmVar(Unit, "x")
            )
          )
        )
      }
    }

    describe("#alphaEquals(term: Term[I]): Boolean") {
      it("should return true if the given term is a product with the equal parameter type and body") {
        val term = TmProd(Unit, "x", TmVar(Unit, "T"), TmVar(Unit, "x"))
        term.alphaEquals(TmProd(Unit, "x", TmVar(Unit, "T"), TmVar(Unit, "x"))) should be (true)
        term.alphaEquals(TmProd(Unit, "z", TmVar(Unit, "T"), TmVar(Unit, "z"))) should be (true)
        term.alphaEquals(TmProd(Unit, "x", TmVar(Unit, "U"), TmVar(Unit, "x"))) should be (false)
        term.alphaEquals(TmProd(Unit, "x", TmVar(Unit, "T"), TmVar(Unit, "y"))) should be (false)
        term.alphaEquals(TmVar(Unit, "x")) should be (false)
        term.alphaEquals(TmConst(Unit, "*")) should be (false)
        term.alphaEquals(TmApp(Unit, TmVar(Unit, "f"), TmVar(Unit, "x"))) should be (false)
        term.alphaEquals(TmAbs(Unit, "x", TmVar(Unit, "T"), TmVar(Unit, "x"))) should be (false)
      }
    }

    describe("substitute(name: String, term: Term[I]): Term[I]") {
      it("should return a product with variables in its parameter type and body substited") {
        {
          val term = TmProd(Unit, "x",
            TmVar(Unit, "T"),
            TmVar(Unit, "x")
          )
          term.substitute("T", TmVar(Unit, "U")) should equal (
            TmProd(Unit, "x",
              TmVar(Unit, "U"),
              TmVar(Unit, "x")
            )
          )
          term.substitute("x", TmVar(Unit, "y")) should equal (
            TmProd(Unit, "x",
              TmVar(Unit, "T"),
              TmVar(Unit, "x")
            )
          )
        }
        {
          val term = TmProd(Unit, "x",
            TmVar(Unit, "T"),
            TmApp(Unit,
              TmVar(Unit, "f"),
              TmVar(Unit, "x")
            )
          )
          term.substitute("f", TmVar(Unit, "g")) should equal (
            TmProd(Unit, "x",
              TmVar(Unit, "T"),
              TmApp(Unit,
                TmVar(Unit, "g"),
                TmVar(Unit, "x")
              )
            )
          )
          term.substitute("f", TmVar(Unit, "x")) should equal (
            TmProd(Unit, "_0",
              TmVar(Unit, "T"),
              TmApp(Unit,
                TmVar(Unit, "x"),
                TmVar(Unit, "_0")
              )
            )
          )
        }
      }
    }
  }

  describe("Term") {
    describe(".normalize[I](env: Env[I], term: Term[I]): Term[I]") {
      it("should perform the full beta reduction on a term and return its normal form") {
        val env = Map(
          "s" -> (TmConst(Unit, "#") -> Some(TmConst(Unit, "*"))),
          "t" -> (TmConst(Unit, "#") -> Some(TmVar(Unit, "s"))),
          "u" -> (TmConst(Unit, "#") -> None),
          "v" -> (TmConst(Unit, "#") -> Some(TmVar(Unit, "u")))
        );
        {
          val term = TmVar(Unit, "x")
          Term.normalize(env, term) should equal (TmVar(Unit, "x"))
        }
        {
          val term = TmVar(Unit, "s")
          Term.normalize(env, term) should equal (TmConst(Unit, "*"))
        }
        {
          val term = TmVar(Unit, "t")
          Term.normalize(env, term) should equal (TmConst(Unit, "*"))
        }
        {
          val term = TmConst(Unit, "#")
          Term.normalize(env, term) should equal (TmConst(Unit, "#"))
        }
        {
          val term = TmApp(Unit, TmVar(Unit, "f"), TmVar(Unit, "x"))
          Term.normalize(env, term) should equal (
            TmApp(Unit, TmVar(Unit, "f"), TmVar(Unit, "x"))
          )
        }
        {
          val term =  TmApp(Unit, TmVar(Unit, "f"), TmVar(Unit, "t"))
          Term.normalize(env, term) should equal (
            TmApp(Unit, TmVar(Unit, "f"), TmConst(Unit, "*"))
          )
        }
        {
          val term = TmApp(Unit, TmVar(Unit, "t"), TmVar(Unit, "x"))
          Term.normalize(env, term) should equal (
            TmApp(Unit, TmConst(Unit, "*"), TmVar(Unit, "x"))
          )
        }
        {
          val term = TmApp(Unit,
            TmAbs(Unit, "x", TmVar(Unit, "T"), TmApp(Unit, TmVar(Unit, "f"), TmVar(Unit, "x"))),
            TmVar(Unit, "y")
          )
          Term.normalize(env, term) should equal (
            TmApp(Unit, TmVar(Unit, "f"), TmVar(Unit, "y"))
          )
        }
        {
          val term = TmApp(Unit,
            TmAbs(Unit, "x", TmVar(Unit, "T"), TmApp(Unit, TmVar(Unit, "f"), TmVar(Unit, "x"))),
            TmVar(Unit, "x")
          )
          Term.normalize(env, term) should equal (
            TmApp(Unit, TmVar(Unit, "f"), TmVar(Unit, "x"))
          )
        }
        {
          val term = TmApp(Unit,
            TmAbs(Unit, "s",
              TmVar(Unit, "T"),
              TmApp(Unit, TmVar(Unit, "t"), TmVar(Unit, "s"))
            ),
            TmVar(Unit, "x")
          )
          Term.normalize(env, term) should equal (
            TmApp(Unit, TmConst(Unit, "*"), TmVar(Unit, "x"))
          )
        }
        {
          val term = TmAbs(Unit, "x",
            TmVar(Unit, "T"),
            TmVar(Unit, "x")
          )
          Term.normalize(env, term) should equal (
            TmAbs(Unit, "x",
              TmVar(Unit, "T"),
              TmVar(Unit, "x")
            )
          )
        }
        {
          val term = TmAbs(Unit, "x",
            TmVar(Unit, "t"),
            TmVar(Unit, "x")
          )
          Term.normalize(env, term) should equal (
            TmAbs(Unit, "x",
              TmConst(Unit, "*"),
              TmVar(Unit, "x")
            )
          )
        }
        {
          val term = TmAbs(Unit, "x",
            TmVar(Unit, "T"),
            TmVar(Unit, "t")
          )
          Term.normalize(env, term) should equal (
            TmAbs(Unit, "x",
              TmVar(Unit, "T"),
              TmConst(Unit, "*")
            )
          )
        }
        {
          val term = TmAbs(Unit, "t",
            TmVar(Unit, "T"),
            TmVar(Unit, "t")
          )
          Term.normalize(env, term) should equal (
            TmAbs(Unit, "_0",
              TmVar(Unit, "T"),
              TmVar(Unit, "_0")
            )
          )
        }
        {
          val term = TmAbs(Unit, "u",
            TmVar(Unit, "T"),
            TmVar(Unit, "v")
          )
          Term.normalize(env, term) should equal (
            TmAbs(Unit, "_0",
              TmVar(Unit, "T"),
              TmVar(Unit, "u")
            )
          )
        }
        {
          val term = TmProd(Unit, "x",
            TmVar(Unit, "T"),
            TmVar(Unit, "x")
          )
          Term.normalize(env, term) should equal (
            TmProd(Unit, "x",
              TmVar(Unit, "T"),
              TmVar(Unit, "x")
            )
          )
        }
        {
          val term = TmProd(Unit, "x",
            TmVar(Unit, "t"),
            TmVar(Unit, "x")
          )
          Term.normalize(env, term) should equal (
            TmProd(Unit, "x",
              TmConst(Unit, "*"),
              TmVar(Unit, "x")
            )
          )
        }
        {
          val term = TmProd(Unit, "x",
            TmVar(Unit, "T"),
            TmVar(Unit, "t")
          )
          Term.normalize(env, term) should equal (
            TmProd(Unit, "x",
              TmVar(Unit, "T"),
              TmConst(Unit, "*")
            )
          )
        }
        {
          val term = TmProd(Unit, "t",
            TmVar(Unit, "T"),
            TmVar(Unit, "t")
          )
          Term.normalize(env, term) should equal (
            TmProd(Unit, "_0",
              TmVar(Unit, "T"),
              TmVar(Unit, "_0")
            )
          )
        }
        {
          val term = TmProd(Unit, "u",
            TmVar(Unit, "T"),
            TmVar(Unit, "v")
          )
          Term.normalize(env, term) should equal (
            TmProd(Unit, "_0",
              TmVar(Unit, "T"),
              TmVar(Unit, "u")
            )
          )
        }
        {
          val term = TmApp(Unit,
            TmApp(Unit,
              TmApp(Unit,
                TmAbs(Unit, "x",
                  TmVar(Unit, "X"),
                  TmAbs(Unit, "y",
                    TmVar(Unit, "Y"),
                    TmAbs(Unit, "z",
                      TmVar(Unit, "Z"),
                      TmApp(Unit,
                        TmApp(Unit, TmVar(Unit, "x"), TmVar(Unit, "z")),
                        TmApp(Unit, TmVar(Unit, "y"), TmVar(Unit, "z"))
                      )
                    )
                  )
                ),
                TmAbs(Unit, "x",
                  TmVar(Unit, "X"),
                  TmAbs(Unit, "y",
                    TmVar(Unit, "Y"),
                    TmVar(Unit, "x")
                  )
                )
              ),
              TmAbs(Unit, "x",
                TmVar(Unit, "X"),
                TmAbs(Unit, "y",
                  TmVar(Unit, "Y"),
                  TmVar(Unit, "x")
                )
              )
            ),
            TmVar(Unit, "x")
          )
          Term.normalize(env, term) should equal(TmVar(Unit, "x"))
        }
      }
    }
    describe(".weakNormalize[I](env: Env[I], term: Term[I]): Term[I]") {
      it("should perform a reduction on a term and return its weak head normal form") {
        val env = Map(
          "s" -> (TmConst(Unit, "#") -> Some(TmConst(Unit, "*"))),
          "t" -> (TmConst(Unit, "#") -> Some(TmVar(Unit, "s"))),
          "u" -> (TmConst(Unit, "#") -> None),
          "v" -> (TmConst(Unit, "#") -> Some(TmVar(Unit, "u")))
        );
        {
          val term = TmVar(Unit, "x")
          Term.weakNormalize(env, term) should equal (TmVar(Unit, "x"))
        }
        {
          val term = TmVar(Unit, "s")
          Term.weakNormalize(env, term) should equal (TmConst(Unit, "*"))
        }
        {
          val term = TmVar(Unit, "t")
          Term.weakNormalize(env, term) should equal (TmConst(Unit, "*"))
        }
        {
          val term = TmConst(Unit, "#")
          Term.weakNormalize(env, term) should equal (TmConst(Unit, "#"))
        }
        {
          val term = TmApp(Unit, TmVar(Unit, "f"), TmVar(Unit, "x"))
          Term.weakNormalize(env, term) should equal (
            TmApp(Unit, TmVar(Unit, "f"), TmVar(Unit, "x"))
          )
        }
        {
          val term =  TmApp(Unit, TmVar(Unit, "f"), TmVar(Unit, "t"))
          Term.weakNormalize(env, term) should equal (
            TmApp(Unit, TmVar(Unit, "f"), TmVar(Unit, "t"))
          )
        }
        {
          val term = TmApp(Unit, TmVar(Unit, "t"), TmVar(Unit, "x"))
          Term.weakNormalize(env, term) should equal (
            TmApp(Unit, TmConst(Unit, "*"), TmVar(Unit, "x"))
          )
        }
        {
          val term = TmApp(Unit,
            TmAbs(Unit, "x", TmVar(Unit, "T"), TmApp(Unit, TmVar(Unit, "f"), TmVar(Unit, "x"))),
            TmVar(Unit, "y")
          )
          Term.weakNormalize(env, term) should equal (
            TmApp(Unit, TmVar(Unit, "f"), TmVar(Unit, "y"))
          )
        }
        {
          val term = TmApp(Unit,
            TmAbs(Unit, "x", TmVar(Unit, "T"), TmApp(Unit, TmVar(Unit, "f"), TmVar(Unit, "x"))),
            TmVar(Unit, "x")
          )
          Term.weakNormalize(env, term) should equal (
            TmApp(Unit, TmVar(Unit, "f"), TmVar(Unit, "x"))
          )
        }
        {
          val term = TmApp(Unit,
            TmAbs(Unit, "s",
              TmVar(Unit, "T"),
              TmApp(Unit, TmVar(Unit, "t"), TmVar(Unit, "s"))
            ),
            TmVar(Unit, "x")
          )
          Term.weakNormalize(env, term) should equal (
            TmApp(Unit, TmConst(Unit, "*"), TmVar(Unit, "x"))
          )
        }
        {
          val term = TmAbs(Unit, "x",
            TmVar(Unit, "T"),
            TmVar(Unit, "x")
          )
          Term.weakNormalize(env, term) should equal (
            TmAbs(Unit, "x",
              TmVar(Unit, "T"),
              TmVar(Unit, "x")
            )
          )
        }
        {
          val term = TmAbs(Unit, "x",
            TmVar(Unit, "t"),
            TmVar(Unit, "x")
          )
          Term.weakNormalize(env, term) should equal (
            TmAbs(Unit, "x",
              TmVar(Unit, "t"),
              TmVar(Unit, "x")
            )
          )
        }
        {
          val term = TmAbs(Unit, "x",
            TmVar(Unit, "T"),
            TmVar(Unit, "t")
          )
          Term.weakNormalize(env, term) should equal (
            TmAbs(Unit, "x",
              TmVar(Unit, "T"),
              TmVar(Unit, "t")
            )
          )
        }
        {
          val term = TmAbs(Unit, "t",
            TmVar(Unit, "T"),
            TmVar(Unit, "t")
          )
          Term.weakNormalize(env, term) should equal (
            TmAbs(Unit, "t",
              TmVar(Unit, "T"),
              TmVar(Unit, "t")
            )
          )
        }
        {
          val term = TmAbs(Unit, "u",
            TmVar(Unit, "T"),
            TmVar(Unit, "v")
          )
          Term.weakNormalize(env, term) should equal (
            TmAbs(Unit, "u",
              TmVar(Unit, "T"),
              TmVar(Unit, "v")
            )
          )
        }
        {
          val term = TmProd(Unit, "x",
            TmVar(Unit, "T"),
            TmVar(Unit, "x")
          )
          Term.weakNormalize(env, term) should equal (
            TmProd(Unit, "x",
              TmVar(Unit, "T"),
              TmVar(Unit, "x")
            )
          )
        }
        {
          val term = TmProd(Unit, "x",
            TmVar(Unit, "t"),
            TmVar(Unit, "x")
          )
          Term.weakNormalize(env, term) should equal (
            TmProd(Unit, "x",
              TmVar(Unit, "t"),
              TmVar(Unit, "x")
            )
          )
        }
        {
          val term = TmProd(Unit, "x",
            TmVar(Unit, "T"),
            TmVar(Unit, "t")
          )
          Term.weakNormalize(env, term) should equal (
            TmProd(Unit, "x",
              TmVar(Unit, "T"),
              TmVar(Unit, "t")
            )
          )
        }
        {
          val term = TmProd(Unit, "t",
            TmVar(Unit, "T"),
            TmVar(Unit, "t")
          )
          Term.weakNormalize(env, term) should equal (
            TmProd(Unit, "t",
              TmVar(Unit, "T"),
              TmVar(Unit, "t")
            )
          )
        }
        {
          val term = TmProd(Unit, "u",
            TmVar(Unit, "T"),
            TmVar(Unit, "v")
          )
          Term.weakNormalize(env, term) should equal (
            TmProd(Unit, "u",
              TmVar(Unit, "T"),
              TmVar(Unit, "v")
            )
          )
        }
        {
          val term = TmApp(Unit,
            TmApp(Unit,
              TmApp(Unit,
                TmAbs(Unit, "x",
                  TmVar(Unit, "X"),
                  TmAbs(Unit, "y",
                    TmVar(Unit, "Y"),
                    TmAbs(Unit, "z",
                      TmVar(Unit, "Z"),
                      TmApp(Unit,
                        TmApp(Unit, TmVar(Unit, "x"), TmVar(Unit, "z")),
                        TmApp(Unit, TmVar(Unit, "y"), TmVar(Unit, "z"))
                      )
                    )
                  )
                ),
                TmAbs(Unit, "x",
                  TmVar(Unit, "X"),
                  TmAbs(Unit, "y",
                    TmVar(Unit, "Y"),
                    TmVar(Unit, "x")
                  )
                )
              ),
              TmAbs(Unit, "x",
                TmVar(Unit, "X"),
                TmAbs(Unit, "y",
                  TmVar(Unit, "Y"),
                  TmVar(Unit, "x")
                )
              )
            ),
            TmVar(Unit, "x")
          )
          Term.weakNormalize(env, term) should equal(TmVar(Unit, "x"))
        }
      }
    }
  }
}
