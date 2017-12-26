package pts

import org.scalatest._

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
}
