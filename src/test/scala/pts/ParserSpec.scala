package pts

import org.scalatest._
import scala.util.parsing.input._

class ParserSpec extends FunSpec with Matchers {
  describe("term") {
    it("should parse a term") {
      {
        val res = Parser.parse(Parser.term, "x")
        res.successful should be (true)
        res.get.alphaEquals(
          TmVar((), "x")
        ) should be (true)
      }
      {
        val res = Parser.parse(Parser.term, "foo_bar_1")
        res.successful should be (true)
        res.get.alphaEquals(
          TmVar((), "foo_bar_1")
        ) should be (true)
      }
      {
        val res = Parser.parse(Parser.term, "_")
        res.successful should be (false)
      }
      {
        val res = Parser.parse(Parser.term, "fun")
        res.successful should be (false)
      }
      {
        val res = Parser.parse(Parser.term, "forall")
        res.successful should be (false)
      }
      {
        val res = Parser.parse(Parser.term, "*")
        res.successful should be (true)
        res.get.alphaEquals(
          TmConst((), "*")
        ) should be (true)
      }
      {
        val res = Parser.parse(Parser.term, "#")
        res.successful should be (true)
        res.get.alphaEquals(
          TmConst((), "#")
        ) should be (true)
      }
      {
        val res = Parser.parse(Parser.term, "?")
        res.successful should be (false)
      }
      {
        val res = Parser.parse(Parser.term, "f x")
        res.successful should be (true)
        res.get.alphaEquals(
          TmApp((),
            TmVar((), "f"),
            TmVar((), "x")
          )
        ) should be (true)
      }
      {
        val res = Parser.parse(Parser.term, "# *")
        res.successful should be (true)
        res.get.alphaEquals(
          TmApp((),
            TmConst((), "#"),
            TmConst((), "*")
          )
        ) should be (true)
      }
      {
        val res = Parser.parse(Parser.term, "f x y")
        res.successful should be (true)
        res.get.alphaEquals(
          TmApp((),
            TmApp((),
              TmVar((), "f"),
              TmVar((), "x")
            ),
            TmVar(() , "y")
          )
        ) should be (true)
      }
      {
        val res = Parser.parse(Parser.term, "f (x y)")
        res.successful should be (true)
        res.get.alphaEquals(
          TmApp((),
            TmVar((), "f"),
            TmApp((),
              TmVar((), "x"),
              TmVar(() , "y")
            )
          )
        ) should be (true)
      }
      {
        val res = Parser.parse(Parser.term, "fun x: T. f x")
        res.successful should be (true)
        res.get.alphaEquals(
          TmAbs((), "x",
            TmVar((), "T"),
            TmApp((),
              TmVar((), "f"),
              TmVar((), "x")
            )
          )
        ) should be (true)
      }
      {
        val res = Parser.parse(Parser.term, "fun _: T. f x")
        res.successful should be (true)
        res.get.alphaEquals(
          TmAbs((), "_",
            TmVar((), "T"),
            TmApp((),
              TmVar((), "f"),
              TmVar((), "x")
            )
          )
        ) should be (true)
      }
      {
        val res = Parser.parse(Parser.term, "forall T: *. P T")
        res.successful should be (true)
        res.get.alphaEquals(
          TmProd((), "T",
            TmConst((), "*"),
            TmApp((),
              TmVar((), "P"),
              TmVar((), "T")
            )
          )
        ) should be (true)
      }
      {
        val res = Parser.parse(Parser.term, "A -> B")
        res.successful should be (true)
        res.get.alphaEquals(
          TmProd((), "_",
            TmVar((), "A"),
            TmVar((), "B")
          )
        ) should be (true)
      }
      {
        val res = Parser.parse(Parser.term, "A -> B -> C")
        res.successful should be (true)
        res.get.alphaEquals(
          TmProd((), "_",
            TmVar((), "A"),
            TmProd((), "_",
              TmVar((), "B"),
              TmVar((), "C")
            )
          )
        ) should be (true)
      }
      {
        val res = Parser.parse(Parser.term, "(A -> B) -> C")
        res.successful should be (true)
        res.get.alphaEquals(
          TmProd((), "_",
            TmProd((), "_",
              TmVar((), "A"),
              TmVar((), "B")
            ),
            TmVar((), "C")
          )
        ) should be (true)
      }
      {
        val res = Parser.parse(Parser.term, "A -> forall x: B. C")
        res.successful should be (true)
        res.get.alphaEquals(
          TmProd((), "_",
            TmVar((), "A"),
            TmProd((), "x",
              TmVar((), "B"),
              TmVar((), "C")
            )
          )
        ) should be (true)
      }
    }
  }

  describe("instruction") {
    it("should parse an instruction") {
      {
        val res = Parser.parse(
          Parser.instruction,
          "assume bottom: forall T: *. T"
        )
        res.successful should be (true)
        res.get.isInstanceOf[InAssume[Position]] should be (true)
        val InAssume(_, name, itsType) = res.get
        name should equal ("bottom")
        itsType.alphaEquals(
          TmProd((), "T",
            TmConst((), "*"),
            TmVar((), "T")
          )
        ) should be (true)
      }
      {
        val res = Parser.parse(
          Parser.instruction,
          "define id = fun T: *. fun x: T. x"
        )
        res.successful should be (true)
        res.get.isInstanceOf[InDefine[Position]] should be (true)
        val InDefine(_, name, itsType, term) = res.get
        name should equal ("id")
        itsType shouldBe empty
        term.alphaEquals(
          TmAbs((), "T",
            TmConst((), "*"),
            TmAbs((), "x",
              TmVar((), "T"),
              TmVar((), "x")
            )
          )
        ) should be (true)
      }
      {
        val res = Parser.parse(
          Parser.instruction,
          "define id: forall T: *. T -> T = fun T: *. fun x: T. x"
        )
        res.successful should be (true)
        res.get.isInstanceOf[InDefine[Position]] should be (true)
        val InDefine(_, name, itsType, term) = res.get
        name should equal ("id")
        itsType should not be empty
        itsType.get.alphaEquals(
          TmProd((), "T",
            TmConst((), "*"),
            TmProd((), "_",
              TmVar((), "T"),
              TmVar((), "T")
            )
          )
        ) should be (true)
        term.alphaEquals(
          TmAbs((), "T",
            TmConst((), "*"),
            TmAbs((), "x",
              TmVar((), "T"),
              TmVar((), "x")
            )
          )
        ) should be (true)
      }
      {
        val res = Parser.parse(
          Parser.instruction,
          "print bottom"
        )
        res.successful should be (true)
        res.get.isInstanceOf[InPrint[Position]] should be (true)
        val InPrint(_, name) = res.get
        name should be ("bottom")
      }
      {
        val res = Parser.parse(
          Parser.instruction,
          "reduce (fun T: *. fun x: T. x) A a"
        )
        res.successful should be (true)
        res.get.isInstanceOf[InReduce[Position]] should be (true)
        val InReduce(_, term) = res.get
        term.alphaEquals(
          TmApp((),
            TmApp((),
              TmAbs((), "T",
                TmConst((), "*"),
                TmAbs((), "x",
                  TmVar((), "T"),
                  TmVar((), "x")
                )
              ),
              TmVar((), "A")
            ),
            TmVar((), "a")
          )
        ) should be (true)
      }
      {
        val res = Parser.parse(
          Parser.instruction,
          "assumebottom: forall T: *. T"
        )
        res.successful should be (false)
      }
    }
  }

  describe("instructions") {
    it("should parse multiple instructions separated by semicolons") {
      {
        val res = Parser.parse(
          Parser.instructions,
          ""
        )
        res.successful should be (true)
        res.get shouldBe empty
      }
      {
        val res = Parser.parse(
          Parser.instructions,
          """
          assume bottom: forall T: *. T;
          """
        )
        res.successful should be (true)
        res.get should have length 1
        res.get(0).isInstanceOf[InAssume[Position]] should be (true)
        val InAssume(_, name, itsType) = res.get(0)
        name should equal ("bottom")
        itsType.alphaEquals(
          TmProd((), "T",
            TmConst((), "*"),
            TmVar((), "T")
          )
        ) should be (true)
      }
      {
        val res = Parser.parse(
          Parser.instructions,
          """
          assume bottom: forall T: *. T;
          define id: forall T: *. T -> T = fun T: *. fun x: T. x;
          print bottom;
          reduce (fun T: *. fun x: T. x) A a;
          """
        )
        res.successful should be (true)
        res.get should have length 4;
        {
          res.get(0).isInstanceOf[InAssume[Position]] should be (true)
          val InAssume(_, name, itsType) = res.get(0)
          name should equal ("bottom")
          itsType.alphaEquals(
            TmProd((), "T",
              TmConst((), "*"),
              TmVar((), "T")
            )
          ) should be (true)
        }
        {
          res.get(1).isInstanceOf[InDefine[Position]] should be (true)
          val InDefine(_, name, itsType, term) = res.get(1)
          name should equal ("id")
          itsType should not be empty
          itsType.get.alphaEquals(
            TmProd((), "T",
              TmConst((), "*"),
              TmProd((), "_",
                TmVar((), "T"),
                TmVar((), "T")
              )
            )
          ) should be (true)
          term.alphaEquals(
            TmAbs((), "T",
              TmConst((), "*"),
              TmAbs((), "x",
                TmVar((), "T"),
                TmVar((), "x")
              )
            )
          ) should be (true)
        }
        {
          res.get(2).isInstanceOf[InPrint[Position]] should be (true)
          val InPrint(_, name) = res.get(2)
          name should be ("bottom")
        }
        {
          res.get(3).isInstanceOf[InReduce[Position]] should be (true)
          val InReduce(_, term) = res.get(3)
          term.alphaEquals(
            TmApp((),
              TmApp((),
                TmAbs((), "T",
                  TmConst((), "*"),
                  TmAbs((), "x",
                    TmVar((), "T"),
                    TmVar((), "x")
                  )
                ),
                TmVar((), "A")
              ),
              TmVar((), "a")
            )
          ) should be (true)
        }
      }
      {
        val res = Parser.parse(
          Parser.instructions,
          """
          assume bottom: forall T: *. T
          """
        )
        res.successful should be (false)
      }
    }
  }
}
