package pts

import scala.collection.immutable.Set
import org.scalatest._

class TermSpec extends FunSpec with Matchers {
  describe("TmVar[I]") {
    describe("#toString(): String") {
      it("should return its name") {
        val term = TmVar(Unit, "x")
        term.toString should be ("x")
      }
    }

    describe("#hasFreeVar(name: String): Boolean") {
      it("should return true if its name is the same as the argument") {
        val term = TmVar(Unit, "x")
        term.hasFreeVar("x") should be (true)
        term.hasFreeVar("y") should be (false)
      }
    }

    describe("#freeVars(): Set[String]") {
      it("should return a singleton set which contains its name") {
        val term = TmVar(Unit, "x")
        term.freeVars should equal (Set("x"))
      }
    }

    describe("#renameFreeVar(oldName: String, newName: String): TmVar[I]") {
      it("should rename its name if it is the same as the specified old name") {
        val term = TmVar(Unit, "x")
        term.renameFreeVar("x", "z") should equal (TmVar(Unit, "z"))
        term.renameFreeVar("y", "z") should equal (TmVar(Unit, "x"))
      }
    }
  }

  describe("TmConst[I]") {
    describe("#toString(): String") {
      it("should return its name") {
        val term = TmConst(Unit, "*")
        term.toString should be ("*")
      }
    }

    describe("#hasFreeVar(name: String): Boolean") {
      it("should always return false") {
        val term = TmConst(Unit, "*")
        term.hasFreeVar("x") should be (false)
        term.hasFreeVar("*") should be (false)
      }
    }

    describe("#freeVars(): Set[String]") {
      it("should always return an empty set") {
        val term = TmConst(Unit, "*")
        term.freeVars should equal (Set.empty)
      }
    }

    describe("#renameFreeVar(oldName: String, newName: String): TmConst[I]") {
      it("should always return itself") {
        val term = TmConst(Unit, "*")
        term.renameFreeVar("x", "z") should equal (TmConst(Unit, "*"))
        term.renameFreeVar("*", "#") should equal (TmConst(Unit, "*"))
      }
    }
  }

  describe("TmApp[I]") {
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

    describe("#hasFreeVar(name: String): Boolean") {
      it("should return true if either its function or argument has the specified variable as free") {
        {
          val term = TmApp(Unit,
            TmVar(Unit, "f"),
            TmVar(Unit, "x")
          )
          term.hasFreeVar("f") should be (true)
          term.hasFreeVar("x") should be (true)
          term.hasFreeVar("y") should be (false)
        }
        {
          val term = TmApp(Unit,
            TmVar(Unit, "x"),
            TmVar(Unit, "x")
          )
          term.hasFreeVar("x") should be (true)
        }
      }
    }

    describe("#freeVars(): Set[String]") {
      it("should return an union set of the free variables of its function and argument") {
        val term = TmApp(Unit,
          TmVar(Unit, "f"),
          TmVar(Unit, "x")
        )
        term.freeVars should equal (Set("f", "x"))
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
  }

  describe("TmAbs[I]") {
    describe("#toString(): String") {
      it("should return a string representation of the abstraction") {
        val term = new TmAbs(Unit, "x",
          TmVar(Unit, "T"),
          TmVar(Unit, "x")
        )
        term.toString should be ("fun x: T. x")
      }
    }

    describe("#hasFreeVar(name: String): Boolean") {
      it("should return true if it has the specified variable as free") {
        {
          val term = new TmAbs(Unit, "x",
            TmVar(Unit, "T"),
            TmApp(Unit,
              TmVar(Unit, "f"),
              TmVar(Unit, "x")
            )
          )
          term.hasFreeVar("x") should be (false)
          term.hasFreeVar("T") should be (true)
          term.hasFreeVar("f") should be (true)
        }
        {
          val term = new TmAbs(Unit, "x",
            TmVar(Unit, "x"),
            TmApp(Unit,
              TmVar(Unit, "f"),
              TmVar(Unit, "x")
            )
          )
          term.hasFreeVar("x") should be (true)
        }
      }
    }

    describe("#freeVars(): Set[String]") {
      it("should return a set of its free variables") {
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
  }

  describe("TmProd[I]") {
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

    describe("#hasFreeVar(name: String): Boolean") {
      it("should return true if it has the specified variable as free") {
        {
          val term = new TmProd(Unit, "x",
            TmVar(Unit, "T"),
            TmApp(Unit,
              TmVar(Unit, "f"),
              TmVar(Unit, "x")
            )
          )
          term.hasFreeVar("x") should be (false)
          term.hasFreeVar("T") should be (true)
          term.hasFreeVar("f") should be (true)
        }
        {
          val term = new TmProd(Unit, "x",
            TmVar(Unit, "x"),
            TmApp(Unit,
              TmVar(Unit, "f"),
              TmVar(Unit, "x")
            )
          )
          term.hasFreeVar("x") should be (true)
        }
      }
    }

    describe("#freeVars(): Set[String]") {
      it("should return a set of its free variables") {
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
  }
}
