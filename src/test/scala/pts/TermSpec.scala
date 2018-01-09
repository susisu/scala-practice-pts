package pts

import scala.collection.immutable._
import org.scalatest._

class TermSpec extends FunSpec with Matchers {
  implicit object UnitInfo extends SourceInfo[Unit] {
    type noInfoType = Unit
    def noInfo = ()
    def showMessage(info: Unit, msg: String): String = msg
  }

  describe("TmVar[I]") {
    describe("#freeVars: Set[String]") {
      it("should be a singleton set which contains its name") {
        val term = TmVar((), "x")
        term.freeVars should equal (Set("x"))
      }
    }

    describe("#toString(): String") {
      it("should return its name") {
        val term = TmVar((), "x")
        term.toString should be ("x")
      }
    }

    describe("#renameFreeVar(oldName: String, newName: String): TmVar[I]") {
      it("should rename its name if it is the same as the specified old name") {
        val term = TmVar((), "x")
        term.renameFreeVar("x", "z") should equal (TmVar((), "z"))
        term.renameFreeVar("y", "z") should equal (TmVar((), "x"))
      }
    }

    describe("#alphaEquals(term: Term[I]): Boolean") {
      it("should return true if the given term is a variable of the same name") {
        val term = TmVar((), "x")
        term.alphaEquals(TmVar((), "x")) should be (true)
        term.alphaEquals(TmVar((), "y")) should be (false)
        term.alphaEquals(TmConst((), "x")) should be (false)
        term.alphaEquals(TmApp((), TmVar((), "f"), TmVar((), "x"))) should be (false)
        term.alphaEquals(TmAbs((), "x", TmVar((), "T"), TmVar((), "x"))) should be (false)
        term.alphaEquals(TmProd((), "x", TmVar((), "T"), TmVar((), "x"))) should be (false)
      }
    }

    describe("substitute(name: String, term: Term[I]): Term[I]") {
      it("should return the given term if and only if the given name is the same as the variable name") {
        val term = TmVar((), "x")
        term.substitute("x", TmVar((), "y")) should equal (TmVar((), "y"))
        term.substitute("y", TmVar((), "z")) should equal (TmVar((), "x"))
      }
    }
  }

  describe("TmConst[I]") {
    describe("#freeVars: Set[String]") {
      it("should be an empty set") {
        val term = TmConst((), "*")
        term.freeVars should equal (Set.empty)
      }
    }

    describe("#toString(): String") {
      it("should return its name") {
        val term = TmConst((), "*")
        term.toString should be ("*")
      }
    }

    describe("#renameFreeVar(oldName: String, newName: String): TmConst[I]") {
      it("should always return itself") {
        val term = TmConst((), "*")
        term.renameFreeVar("x", "z") should equal (TmConst((), "*"))
        term.renameFreeVar("*", "#") should equal (TmConst((), "*"))
      }
    }

    describe("#alphaEquals(term: Term[I]): Boolean") {
      it("should return true if the given term is a constant of the same name") {
        val term = TmConst((), "*")
        term.alphaEquals(TmConst((), "*")) should be (true)
        term.alphaEquals(TmConst((), "#")) should be (false)
        term.alphaEquals(TmVar((), "*")) should be (false)
        term.alphaEquals(TmApp((), TmVar((), "f"), TmVar((), "x"))) should be (false)
        term.alphaEquals(TmAbs((), "x", TmVar((), "T"), TmVar((), "x"))) should be (false)
        term.alphaEquals(TmProd((), "x", TmVar((), "T"), TmVar((), "x"))) should be (false)
      }
    }

    describe("substitute(name: String, term: Term[I]): Term[I]") {
      it("should always return itself") {
        val term = TmConst((), "*")
        term.substitute("x", TmVar((), "y")) should equal (TmConst((), "*"))
        term.substitute("*", TmVar((), "x")) should equal (TmConst((), "*"))
      }
    }
  }

  describe("TmApp[I]") {
    describe("#freeVars: Set[String]") {
      it("should be an union set of the free variables of its function and argument") {
        val term = TmApp((),
          TmVar((), "f"),
          TmVar((), "x")
        )
        term.freeVars should equal (Set("f", "x"))
      }
    }

    describe("#toString(): String") {
      it("should return a string representation of the application") {
        {
          val term = TmApp((),
            TmVar((), "f"),
            TmVar((), "x")
          )
          term.toString should be ("f x")
        }
        {
          val term = TmApp((),
            TmConst((), "*"),
            TmVar((), "x")
          )
          term.toString should be ("* x")
        }
        {
          val term = TmApp((),
            TmApp((),
              TmVar((), "f"),
              TmVar((), "x")
            ),
            TmVar((), "y")
          )
          term.toString should be ("f x y")
        }
        {
          val term = TmApp((),
            TmAbs((), "x",
              TmVar((), "T"),
              TmVar((), "x")
            ),
            TmVar((), "y")
          )
          term.toString should be ("(fun x: T. x) y")
        }
        {
          val term = TmApp((),
            TmProd((), "T",
              TmConst((), "*"),
              TmVar((), "T")
            ),
            TmVar((), "U")
          )
          term.toString should be ("(forall T: *. T) U")
        }
        {
          val term = TmApp((),
            TmVar((), "f"),
            TmConst((), "*")
          )
          term.toString should be ("f *")
        }
        {
          val term = TmApp((),
            TmVar((), "f"),
            TmApp((),
              TmVar((), "x"),
              TmVar((), "y")
            )
          )
          term.toString should be ("f (x y)")
        }
        {
          val term = TmApp((),
            TmVar((), "f"),
            TmAbs((), "x",
              TmVar((), "T"),
              TmVar((), "x")
            )
          )
          term.toString should be ("f (fun x: T. x)")
        }
        {
          val term = TmApp((),
            TmVar((), "f"),
            TmProd((), "T",
              TmConst((), "*"),
              TmVar((), "T")
            )
          )
          term.toString should be ("f (forall T: *. T)")
        }
      }
    }

    describe("#renameFreeVar(oldName: String, newName: String): TmApp[I]") {
      it("should rename free variables in the function and the argument") {
        {
          val term = TmApp((),
            TmVar((), "f"),
            TmVar((), "x")
          )
          term.renameFreeVar("f", "g") should equal (
            TmApp((),
              TmVar((), "g"),
              TmVar((), "x")
            )
          )
          term.renameFreeVar("x", "z") should equal (
            TmApp((),
              TmVar((), "f"),
              TmVar((), "z")
            )
          )
          term.renameFreeVar("y", "z") should equal (
            TmApp((),
              TmVar((), "f"),
              TmVar((), "x")
            )
          )
        }
        {
          val term = TmApp((),
            TmVar((), "x"),
            TmVar((), "x")
          )
          term.renameFreeVar("x", "z") should equal (
            TmApp((),
              TmVar((), "z"),
              TmVar((), "z")
            )
          )
        }
      }
    }

    describe("#alphaEquals(term: Term[I]): Boolean") {
      it("should return true if the given term is an application of the equal function and argument") {
        val term = TmApp((), TmVar((), "f"), TmVar((), "x"))
        term.alphaEquals(TmApp((), TmVar((), "f"), TmVar((), "x"))) should be (true)
        term.alphaEquals(TmApp((), TmVar((), "g"), TmVar((), "x"))) should be (false)
        term.alphaEquals(TmApp((), TmVar((), "f"), TmVar((), "y"))) should be (false)
        term.alphaEquals(TmVar((), "x")) should be (false)
        term.alphaEquals(TmConst((), "*")) should be (false)
        term.alphaEquals(TmAbs((), "x", TmVar((), "T"), TmVar((), "x"))) should be (false)
        term.alphaEquals(TmProd((), "x", TmVar((), "T"), TmVar((), "x"))) should be (false)
      }
    }

    describe("substitute(name: String, term: Term[I]): Term[I]") {
      it("should return an application with variables in its function and argument substited") {
        {
          val term = TmApp((),
            TmVar((), "f"),
            TmVar((), "x")
          )
          term.substitute("f", TmVar((), "g")) should equal (
            TmApp((),
              TmVar((), "g"),
              TmVar((), "x")
            )
          )
          term.substitute("x", TmVar((), "y")) should equal (
            TmApp((),
              TmVar((), "f"),
              TmVar((), "y")
            )
          )
          term.substitute("y", TmVar((), "z")) should equal (
            TmApp((),
              TmVar((), "f"),
              TmVar((), "x")
            )
          )
        }
        {
          val term = TmApp((),
            TmVar((), "x"),
            TmVar((), "x")
          )
          term.substitute("x", TmVar((), "y")) should equal (
            TmApp((),
              TmVar((), "y"),
              TmVar((), "y")
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
          val term = new TmAbs((), "x",
            TmVar((), "T"),
            TmApp((),
              TmVar((), "f"),
              TmVar((), "x")
            )
          )
          term.freeVars should equal (Set("T", "f"))
        }
        {
          val term = new TmAbs((), "x",
            TmVar((), "x"),
            TmApp((),
              TmVar((), "f"),
              TmVar((), "x")
            )
          )
          term.freeVars should equal (Set("x", "f"))
        }
      }
    }

    describe("#toString(): String") {
      it("should return a string representation of the abstraction") {
        val term = new TmAbs((), "x",
          TmVar((), "T"),
          TmVar((), "x")
        )
        term.toString should be ("fun x: T. x")
      }
    }

    describe("#renameFreeVar(oldName: String, newName: String): TmAbs[I]") {
      it("should rename free variables in the parameter type and the body") {
        val term = new TmAbs((), "x",
          TmVar((), "T"),
          TmApp((),
            TmVar((), "f"),
            TmVar((), "x")
          )
        )
        term.renameFreeVar("T", "U") should equal (
          new TmAbs((), "x",
            TmVar((), "U"),
            TmApp((),
              TmVar((), "f"),
              TmVar((), "x")
            )
          )
        )
        term.renameFreeVar("f", "g") should equal (
          new TmAbs((), "x",
            TmVar((), "T"),
            TmApp((),
              TmVar((), "g"),
              TmVar((), "x")
            )
          )
        )
        term.renameFreeVar("x", "y") should equal (
          new TmAbs((), "x",
            TmVar((), "T"),
            TmApp((),
              TmVar((), "f"),
              TmVar((), "x")
            )
          )
        )
        term.renameFreeVar("y", "z") should equal (
          new TmAbs((), "x",
            TmVar((), "T"),
            TmApp((),
              TmVar((), "f"),
              TmVar((), "x")
            )
          )
        )
        term.renameFreeVar("f", "x") should equal (
          new TmAbs((), "_0",
            TmVar((), "T"),
            TmApp((),
              TmVar((), "x"),
              TmVar((), "_0")
            )
          )
        )
      }
    }

    describe("#alphaEquals(term: Term[I]): Boolean") {
      it("should return true if the given term is an abstraction with the equal parameter type and body") {
        val term = TmAbs((), "x", TmVar((), "T"), TmVar((), "x"))
        term.alphaEquals(TmAbs((), "x", TmVar((), "T"), TmVar((), "x"))) should be (true)
        term.alphaEquals(TmAbs((), "z", TmVar((), "T"), TmVar((), "z"))) should be (true)
        term.alphaEquals(TmAbs((), "x", TmVar((), "U"), TmVar((), "x"))) should be (false)
        term.alphaEquals(TmAbs((), "x", TmVar((), "T"), TmVar((), "y"))) should be (false)
        term.alphaEquals(TmVar((), "x")) should be (false)
        term.alphaEquals(TmConst((), "*")) should be (false)
        term.alphaEquals(TmApp((), TmVar((), "f"), TmVar((), "x"))) should be (false)
        term.alphaEquals(TmProd((), "x", TmVar((), "T"), TmVar((), "x"))) should be (false)
      }
    }

    describe("substitute(name: String, term: Term[I]): Term[I]") {
      it("should return an abstraction with variables in its parameter type and body substited") {
        {
          val term = TmAbs((), "x",
            TmVar((), "T"),
            TmVar((), "x")
          )
          term.substitute("T", TmVar((), "U")) should equal (
            TmAbs((), "x",
              TmVar((), "U"),
              TmVar((), "x")
            )
          )
          term.substitute("x", TmVar((), "y")) should equal (
            TmAbs((), "x",
              TmVar((), "T"),
              TmVar((), "x")
            )
          )
        }
        {
          val term = TmAbs((), "x",
            TmVar((), "T"),
            TmApp((),
              TmVar((), "f"),
              TmVar((), "x")
            )
          )
          term.substitute("f", TmVar((), "g")) should equal (
            TmAbs((), "x",
              TmVar((), "T"),
              TmApp((),
                TmVar((), "g"),
                TmVar((), "x")
              )
            )
          )
          term.substitute("f", TmVar((), "x")) should equal (
            TmAbs((), "_0",
              TmVar((), "T"),
              TmApp((),
                TmVar((), "x"),
                TmVar((), "_0")
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
          val term = new TmProd((), "x",
            TmVar((), "T"),
            TmApp((),
              TmVar((), "f"),
              TmVar((), "x")
            )
          )
          term.freeVars should equal (Set("T", "f"))
        }
        {
          val term = new TmProd((), "x",
            TmVar((), "x"),
            TmApp((),
              TmVar((), "f"),
              TmVar((), "x")
            )
          )
          term.freeVars should equal (Set("x", "f"))
        }
      }
    }

    describe("#toString(): String") {
      it("should return a string representation of the product") {
        {
          val term = new TmProd((), "_",
            TmVar((), "T"),
            TmVar((), "U")
          )
          term.toString should be ("T -> U")
        }
        {
          val term = new TmProd((), "_",
            TmConst((), "*"),
            TmVar((), "T")
          )
          term.toString should be ("* -> T")
        }
        {
          val term = new TmProd((), "_",
            TmApp((),
              TmVar((), "F"),
              TmVar((), "T")
            ),
            TmVar((), "U")
          )
          term.toString should be ("F T -> U")
        }
        {
          val term = new TmProd((), "_",
            TmAbs((), "T",
              TmConst((), "*"),
              TmVar((), "T")
            ),
            TmVar((), "U")
          )
          term.toString should be ("(fun T: *. T) -> U")
        }
        {
          val term = new TmProd((), "_",
            TmProd((), "T",
              TmConst((), "*"),
              TmVar((), "T")
            ),
            TmVar((), "U")
          )
          term.toString should be ("(forall T: *. T) -> U")
        }
        {
          val term = new TmProd((), "_",
            TmVar((), "T"),
            TmAbs((), "U",
              TmConst((), "*"),
              TmVar((), "U")
            )
          )
          term.toString should be ("T -> fun U: *. U")
        }
        {
          val term = new TmProd((), "_",
            TmVar((), "T"),
            TmProd((), "U",
              TmConst((), "*"),
              TmVar((), "U")
            )
          )
          term.toString should be ("T -> forall U: *. U")
        }
        {
          val term = new TmProd((), "T",
            TmConst((), "*"),
            TmVar((), "T")
          )
          term.toString should be ("forall T: *. T")
        }
      }
    }

    describe("#renameFreeVar(oldName: String, newName: String): TmProd[I]") {
      it("should rename free variables in the parameter type and the body") {
        val term = new TmProd((), "x",
          TmVar((), "T"),
          TmApp((),
            TmVar((), "f"),
            TmVar((), "x")
          )
        )
        term.renameFreeVar("T", "U") should equal (
          new TmProd((), "x",
            TmVar((), "U"),
            TmApp((),
              TmVar((), "f"),
              TmVar((), "x")
            )
          )
        )
        term.renameFreeVar("f", "g") should equal (
          new TmProd((), "x",
            TmVar((), "T"),
            TmApp((),
              TmVar((), "g"),
              TmVar((), "x")
            )
          )
        )
        term.renameFreeVar("x", "y") should equal (
          new TmProd((), "x",
            TmVar((), "T"),
            TmApp((),
              TmVar((), "f"),
              TmVar((), "x")
            )
          )
        )
        term.renameFreeVar("y", "z") should equal (
          new TmProd((), "x",
            TmVar((), "T"),
            TmApp((),
              TmVar((), "f"),
              TmVar((), "x")
            )
          )
        )
        term.renameFreeVar("f", "x") should equal (
          new TmProd((), "x0",
            TmVar((), "T"),
            TmApp((),
              TmVar((), "x"),
              TmVar((), "x0")
            )
          )
        )
      }
    }

    describe("#alphaEquals(term: Term[I]): Boolean") {
      it("should return true if the given term is a product with the equal parameter type and body") {
        val term = TmProd((), "x", TmVar((), "T"), TmVar((), "x"))
        term.alphaEquals(TmProd((), "x", TmVar((), "T"), TmVar((), "x"))) should be (true)
        term.alphaEquals(TmProd((), "z", TmVar((), "T"), TmVar((), "z"))) should be (true)
        term.alphaEquals(TmProd((), "x", TmVar((), "U"), TmVar((), "x"))) should be (false)
        term.alphaEquals(TmProd((), "x", TmVar((), "T"), TmVar((), "y"))) should be (false)
        term.alphaEquals(TmVar((), "x")) should be (false)
        term.alphaEquals(TmConst((), "*")) should be (false)
        term.alphaEquals(TmApp((), TmVar((), "f"), TmVar((), "x"))) should be (false)
        term.alphaEquals(TmAbs((), "x", TmVar((), "T"), TmVar((), "x"))) should be (false)
      }
    }

    describe("substitute(name: String, term: Term[I]): Term[I]") {
      it("should return a product with variables in its parameter type and body substited") {
        {
          val term = TmProd((), "x",
            TmVar((), "T"),
            TmVar((), "x")
          )
          term.substitute("T", TmVar((), "U")) should equal (
            TmProd((), "x",
              TmVar((), "U"),
              TmVar((), "x")
            )
          )
          term.substitute("x", TmVar((), "y")) should equal (
            TmProd((), "x",
              TmVar((), "T"),
              TmVar((), "x")
            )
          )
        }
        {
          val term = TmProd((), "x",
            TmVar((), "T"),
            TmApp((),
              TmVar((), "f"),
              TmVar((), "x")
            )
          )
          term.substitute("f", TmVar((), "g")) should equal (
            TmProd((), "x",
              TmVar((), "T"),
              TmApp((),
                TmVar((), "g"),
                TmVar((), "x")
              )
            )
          )
          term.substitute("f", TmVar((), "x")) should equal (
            TmProd((), "x0",
              TmVar((), "T"),
              TmApp((),
                TmVar((), "x"),
                TmVar((), "x0")
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
          "T" -> (TmConst((), "*") -> None),
          "x" -> (TmVar((), "T") -> None),
          "y" -> (TmVar((), "T") -> None),
          "f" -> (TmProd((), "x", TmVar((), "T"), TmVar((), "T")) -> None),
          "U" -> (TmConst((), "*") -> Some(TmVar((), "T"))),
          "z" -> (TmVar((), "T") -> Some(TmVar((), "x"))),
          "w" -> (TmVar((), "T") -> Some(TmVar((), "z"))),
          "g" -> (TmProd((), "x", TmVar((), "T"), TmVar((), "T")) -> Some(TmVar((), "f"))),
        );
        {
          val term = TmVar((), "t")
          Term.normalize(env, term).left.get should include ("not declared")
        }
        {
          val term = TmVar((), "x")
          Term.normalize(env, term).right.get should equal (TmVar((), "x"))
        }
        {
          val term = TmVar((), "z")
          Term.normalize(env, term).right.get should equal (TmVar((), "x"))
        }
        {
          val term = TmVar((), "w")
          Term.normalize(env, term).right.get should equal (TmVar((), "x"))
        }
        {
          val term = TmConst((), "#")
          Term.normalize(env, term).right.get should equal (TmConst((), "#"))
        }
        {
          val term = TmApp((), TmVar((), "f"), TmVar((), "x"))
          Term.normalize(env, term).right.get should equal (
            TmApp((), TmVar((), "f"), TmVar((), "x"))
          )
        }
        {
          val term =  TmApp((), TmVar((), "f"), TmVar((), "z"))
          Term.normalize(env, term).right.get should equal (
            TmApp((), TmVar((), "f"), TmVar((), "x"))
          )
        }
        {
          val term = TmApp((), TmVar((), "g"), TmVar((), "x"))
          Term.normalize(env, term).right.get should equal (
            TmApp((), TmVar((), "f"), TmVar((), "x"))
          )
        }
        {
          val term = TmApp((),
            TmAbs((), "t", TmVar((), "T"), TmApp((), TmVar((), "f"), TmVar((), "t"))),
            TmVar((), "y")
          )
          Term.normalize(env, term).right.get should equal (
            TmApp((), TmVar((), "f"), TmVar((), "y"))
          )
        }
        {
          val term = TmApp((),
            TmAbs((), "x",
              TmVar((), "T"),
              TmApp((), TmVar((), "f"), TmVar((), "x"))
            ),
            TmVar((), "x")
          )
          Term.normalize(env, term).right.get should equal (
            TmApp((), TmVar((), "f"), TmVar((), "x"))
          )
        }
        {
          val term = TmApp((),
            TmAbs((), "t",
              TmVar((), "T"),
              TmApp((), TmVar((), "g"), TmVar((), "t"))
            ),
            TmVar((), "x")
          )
          Term.normalize(env, term).right.get should equal (
            TmApp((), TmVar((), "f"), TmVar((), "x"))
          )
        }
        {
          val term = TmAbs((), "t",
            TmVar((), "T"),
            TmVar((), "t")
          )
          Term.normalize(env, term).right.get should equal (
            TmAbs((), "t",
              TmVar((), "T"),
              TmVar((), "t")
            )
          )
        }
        {
          val term = TmAbs((), "t",
            TmVar((), "U"),
            TmVar((), "t")
          )
          Term.normalize(env, term).right.get should equal (
            TmAbs((), "t",
              TmVar((), "T"),
              TmVar((), "t")
            )
          )
        }
        {
          val term = TmAbs((), "t",
            TmVar((), "T"),
            TmVar((), "z")
          )
          Term.normalize(env, term).right.get should equal (
            TmAbs((), "t",
              TmVar((), "T"),
              TmVar((), "x")
            )
          )
        }
        {
          val term = TmAbs((), "x",
            TmVar((), "T"),
            TmVar((), "x")
          )
          Term.normalize(env, term).right.get should equal (
            TmAbs((), "x0",
              TmVar((), "T"),
              TmVar((), "x0")
            )
          )
        }
        {
          val term = TmAbs((), "x",
            TmVar((), "T"),
            TmVar((), "z")
          )
          Term.normalize(env, term).right.get should equal (
            TmAbs((), "x0",
              TmVar((), "T"),
              TmVar((), "x")
            )
          )
        }
        {
          val term = TmProd((), "t",
            TmVar((), "T"),
            TmVar((), "t")
          )
          Term.normalize(env, term).right.get should equal (
            TmProd((), "t",
              TmVar((), "T"),
              TmVar((), "t")
            )
          )
        }
        {
          val term = TmProd((), "t",
            TmVar((), "U"),
            TmVar((), "t")
          )
          Term.normalize(env, term).right.get should equal (
            TmProd((), "t",
              TmVar((), "T"),
              TmVar((), "t")
            )
          )
        }
        {
          val term = TmProd((), "t",
            TmVar((), "T"),
            TmVar((), "z")
          )
          Term.normalize(env, term).right.get should equal (
            TmProd((), "t",
              TmVar((), "T"),
              TmVar((), "x")
            )
          )
        }
        {
          val term = TmProd((), "x",
            TmVar((), "T"),
            TmVar((), "x")
          )
          Term.normalize(env, term).right.get should equal (
            TmProd((), "x0",
              TmVar((), "T"),
              TmVar((), "x0")
            )
          )
        }
        {
          val term = TmProd((), "x",
            TmVar((), "T"),
            TmVar((), "z")
          )
          Term.normalize(env, term).right.get should equal (
            TmProd((), "x0",
              TmVar((), "T"),
              TmVar((), "x")
            )
          )
        }
        {
          val term = TmApp((),
            TmApp((),
              TmApp((),
                TmAbs((), "x",
                  TmProd((), "x", TmVar((), "T"), TmProd((), "w", TmVar((), "T"), TmVar((), "T"))),
                  TmAbs((), "y",
                    TmProd((), "z", TmVar((), "T"), TmVar((), "T")),
                    TmAbs((), "z",
                      TmVar((), "T"),
                      TmApp((),
                        TmApp((), TmVar((), "x"), TmVar((), "z")),
                        TmApp((), TmVar((), "y"), TmVar((), "z"))
                      )
                    )
                  )
                ),
                TmAbs((), "x",
                  TmVar((), "T"),
                  TmAbs((), "y",
                    TmVar((), "T"),
                    TmVar((), "x")
                  )
                )
              ),
              TmAbs((), "x",
                TmVar((), "T"),
                TmAbs((), "y",
                  TmVar((), "T"),
                  TmVar((), "x")
                )
              )
            ),
            TmVar((), "x")
          )
          Term.normalize(env, term).right.get should equal(TmVar((), "x"))
        }
      }
    }
    describe(".weakNormalize[I](env: Env[I], term: Term[I]): Term[I]") {
      it("should perform a reduction on a term and return its weak head normal form") {
        val env = Map(
          "T" -> (TmConst((), "*") -> None),
          "x" -> (TmVar((), "T") -> None),
          "y" -> (TmVar((), "T") -> None),
          "f" -> (TmProd((), "x", TmVar((), "T"), TmVar((), "T")) -> None),
          "U" -> (TmConst((), "*") -> Some(TmVar((), "T"))),
          "z" -> (TmVar((), "T") -> Some(TmVar((), "x"))),
          "w" -> (TmVar((), "T") -> Some(TmVar((), "z"))),
          "g" -> (TmProd((), "x", TmVar((), "T"), TmVar((), "T")) -> Some(TmVar((), "f"))),
        );
        {
          val term = TmVar((), "t")
          Term.weakNormalize(env, term).left.get should include ("not declared")
        }
        {
          val term = TmVar((), "x")
          Term.weakNormalize(env, term).right.get should equal (TmVar((), "x"))
        }
        {
          val term = TmVar((), "z")
          Term.weakNormalize(env, term).right.get should equal (TmVar((), "x"))
        }
        {
          val term = TmVar((), "w")
          Term.weakNormalize(env, term).right.get should equal (TmVar((), "x"))
        }
        {
          val term = TmConst((), "#")
          Term.weakNormalize(env, term).right.get should equal (TmConst((), "#"))
        }
        {
          val term = TmApp((), TmVar((), "f"), TmVar((), "x"))
          Term.weakNormalize(env, term).right.get should equal (
            TmApp((), TmVar((), "f"), TmVar((), "x"))
          )
        }
        {
          val term =  TmApp((), TmVar((), "f"), TmVar((), "z"))
          Term.weakNormalize(env, term).right.get should equal (
            TmApp((), TmVar((), "f"), TmVar((), "z"))
          )
        }
        {
          val term = TmApp((), TmVar((), "g"), TmVar((), "x"))
          Term.weakNormalize(env, term).right.get should equal (
            TmApp((), TmVar((), "f"), TmVar((), "x"))
          )
        }
        {
          val term = TmApp((),
            TmAbs((), "x", TmVar((), "T"), TmApp((), TmVar((), "f"), TmVar((), "x"))),
            TmVar((), "y")
          )
          Term.weakNormalize(env, term).right.get should equal (
            TmApp((), TmVar((), "f"), TmVar((), "y"))
          )
        }
        {
          val term = TmApp((),
            TmAbs((), "x", TmVar((), "T"), TmApp((), TmVar((), "f"), TmVar((), "x"))),
            TmVar((), "x")
          )
          Term.weakNormalize(env, term).right.get should equal (
            TmApp((), TmVar((), "f"), TmVar((), "x"))
          )
        }
        {
          val term = TmApp((),
            TmAbs((), "t",
              TmVar((), "T"),
              TmApp((), TmVar((), "g"), TmVar((), "t"))
            ),
            TmVar((), "y")
          )
          Term.weakNormalize(env, term).right.get should equal (
            TmApp((), TmVar((), "f"), TmVar((), "y"))
          )
        }
        {
          val term = TmAbs((), "t",
            TmVar((), "T"),
            TmVar((), "t")
          )
          Term.weakNormalize(env, term).right.get should equal (
            TmAbs((), "t",
              TmVar((), "T"),
              TmVar((), "t")
            )
          )
        }
        {
          val term = TmAbs((), "t",
            TmVar((), "U"),
            TmVar((), "t")
          )
          Term.weakNormalize(env, term).right.get should equal (
            TmAbs((), "t",
              TmVar((), "U"),
              TmVar((), "t")
            )
          )
        }
        {
          val term = TmAbs((), "t",
            TmVar((), "T"),
            TmVar((), "z")
          )
          Term.weakNormalize(env, term).right.get should equal (
            TmAbs((), "t",
              TmVar((), "T"),
              TmVar((), "z")
            )
          )
        }
        {
          val term = TmAbs((), "x",
            TmVar((), "T"),
            TmVar((), "x")
          )
          Term.weakNormalize(env, term).right.get should equal (
            TmAbs((), "x",
              TmVar((), "T"),
              TmVar((), "x")
            )
          )
        }
        {
          val term = TmAbs((), "x",
            TmVar((), "T"),
            TmVar((), "z")
          )
          Term.weakNormalize(env, term).right.get should equal (
            TmAbs((), "x",
              TmVar((), "T"),
              TmVar((), "z")
            )
          )
        }
        {
          val term = TmProd((), "t",
            TmVar((), "T"),
            TmVar((), "t")
          )
          Term.weakNormalize(env, term).right.get should equal (
            TmProd((), "t",
              TmVar((), "T"),
              TmVar((), "t")
            )
          )
        }
        {
          val term = TmProd((), "t",
            TmVar((), "U"),
            TmVar((), "t")
          )
          Term.weakNormalize(env, term).right.get should equal (
            TmProd((), "t",
              TmVar((), "U"),
              TmVar((), "t")
            )
          )
        }
        {
          val term = TmProd((), "t",
            TmVar((), "T"),
            TmVar((), "z")
          )
          Term.weakNormalize(env, term).right.get should equal (
            TmProd((), "t",
              TmVar((), "T"),
              TmVar((), "z")
            )
          )
        }
        {
          val term = TmProd((), "x",
            TmVar((), "T"),
            TmVar((), "x")
          )
          Term.weakNormalize(env, term).right.get should equal (
            TmProd((), "x",
              TmVar((), "T"),
              TmVar((), "x")
            )
          )
        }
        {
          val term = TmProd((), "x",
            TmVar((), "T"),
            TmVar((), "z")
          )
          Term.weakNormalize(env, term).right.get should equal (
            TmProd((), "x",
              TmVar((), "T"),
              TmVar((), "z")
            )
          )
        }
        {
          val term = TmApp((),
            TmApp((),
              TmApp((),
                TmAbs((), "x",
                  TmProd((), "x", TmVar((), "T"), TmProd((), "w", TmVar((), "T"), TmVar((), "T"))),
                  TmAbs((), "y",
                    TmProd((), "z", TmVar((), "T"), TmVar((), "T")),
                    TmAbs((), "z",
                      TmVar((), "T"),
                      TmApp((),
                        TmApp((), TmVar((), "x"), TmVar((), "z")),
                        TmApp((), TmVar((), "y"), TmVar((), "z"))
                      )
                    )
                  )
                ),
                TmAbs((), "x",
                  TmVar((), "T"),
                  TmAbs((), "y",
                    TmVar((), "T"),
                    TmVar((), "x")
                  )
                )
              ),
              TmAbs((), "x",
                TmVar((), "T"),
                TmAbs((), "y",
                  TmVar((), "T"),
                  TmVar((), "x")
                )
              )
            ),
            TmVar((), "x")
          )
          Term.weakNormalize(env, term).right.get should equal(TmVar((), "x"))
        }
      }
    }
  }
}
