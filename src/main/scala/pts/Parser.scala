package pts

import scala.util.parsing.combinator._
import scala.util.parsing.input._

object Parser extends RegexParsers {
  sealed abstract trait Token extends Positional
  case class TkLParen() extends Token
  case class TkRParen() extends Token
  case class TkFun() extends Token
  case class TkForall() extends Token
  case class TkDot() extends Token
  case class TkColon() extends Token
  case class TkArrow() extends Token
  case class TkUnder() extends Token
  case class TkIdent(name: String) extends Token
  case class TkConst(name: String) extends Token
  case class TkAssume() extends Token
  case class TkDefine() extends Token
  case class TkPrint() extends Token
  case class TkCompute() extends Token
  case class TkEqual() extends Token

  override def skipWhitespace = true
  override val whiteSpace = "[ \t\r\n\f]+".r

  val reservedNames: Set[String] = Set("fun", "forall")

  def tkLParen: Parser[TkLParen] = positioned ("("      ^^ { _ => TkLParen() })
  def tkRParen: Parser[TkRParen] = positioned (")"      ^^ { _ => TkRParen() })
  def tkFun   : Parser[TkFun]    = positioned ("fun"    ^^ { _ => TkFun() })
  def tkForall: Parser[TkFun]    = positioned ("forall" ^^ { _ => TkFun() })
  def tkDot   : Parser[TkDot]    = positioned ("."      ^^ { _ => TkDot() })
  def tkColon : Parser[TkColon]  = positioned (":"      ^^ { _ => TkColon() })
  def tkArrow : Parser[TkArrow]  = positioned ("->"     ^^ { _ => TkArrow() })
  def tkUnder : Parser[TkUnder]  = positioned ("_"      ^^ { _ => TkUnder() })

  def tkIdent: Parser[TkIdent] = positioned (
    "[A-Za-z][A-Za-z0-9_]*".r.filter { !reservedNames.contains(_) } ^^ { TkIdent(_) }
  )
  def tkConst: Parser[TkConst] = positioned (raw"[\*#]".r ^^ { TkConst(_) })

  def tkAssume : Parser[TkAssume] = positioned ("assume"  ^^ { _ => TkAssume() })
  def tkDefine : Parser[TkDefine] = positioned ("define"  ^^ { _ => TkDefine() })
  def tkPrint  : Parser[TkDefine] = positioned ("print"   ^^ { _ => TkDefine() })
  def tkCompute: Parser[TkDefine] = positioned ("compute" ^^ { _ => TkDefine() })
  def tkEqual  : Parser[TkEqual]  = positioned ("="       ^^ { _ => TkEqual() })

  def pattern: Parser[String] =
      tkUnder ^^^ "_" |
      tkIdent ^^ { _.name }

  def tmVar: Parser[TmVar[Position]] =
    tkIdent ^^ { token => TmVar(token.pos, token.name) }
  def tmConst: Parser[TmConst[Position]] =
    tkConst ^^ { token => TmConst(token.pos, token.name) }
  def aterm: Parser[Term[Position]] =
    tmVar |
    tmConst |
    tkLParen ~> term <~ tkRParen
  def tmApp: Parser[Term[Position]] =
    aterm ~ rep(aterm) ^^ {
      case func ~ args => args.foldLeft(func) {
        case (acc, arg) => TmApp(arg.info, acc, arg)
      }
    }
  def tmAbs: Parser[TmAbs[Position]] =
    tkFun ~ pattern ~ (tkColon ~> term) ~ (tkDot ~> term) ^^ {
      case head ~ paramName ~ paramType ~ body =>
        TmAbs(head.pos, paramName, paramType, body)
    }
  def tmProd: Parser[TmProd[Position]] =
    tkForall ~ pattern ~ (tkColon ~> term) ~ (tkDot ~> term) ^^ {
      case head ~ paramName ~ paramType ~ body =>
        TmProd(head.pos, paramName, paramType, body)
    }
  def tmArrow: Parser[Term[Position]] =
    tmApp ~ opt(tkArrow ~ term) ^^ {
      case dom ~ Some(arr ~ codom) => TmProd(arr.pos, "_", dom, codom)
      case tm ~ None => tm
    }
  def term: Parser[Term[Position]] = tmAbs | tmProd | tmArrow

  def inAssume: Parser[InAssume[Position]] =
    tkAssume ~ tkIdent ~ (tkColon ~> term) ^^ {
      case head ~ ident ~ itsType => InAssume(head.pos, ident.name, itsType)
    }
  def inDefine: Parser[InDefine[Position]] =
    tkDefine ~ tkIdent ~ opt (tkColon ~> term) ~ (tkEqual ~> term) ^^ {
      case head ~ ident ~ itsType ~ term => InDefine(head.pos, ident.name, itsType, term)
    }
  def inPrint: Parser[InPrint[Position]] =
    tkPrint ~ tkIdent ^^ {
      case head ~ ident => InPrint(head.pos, ident.name)
    }
  def inCompute: Parser[InCompute[Position]] =
    tkCompute ~ term ^^ {
      case head ~ term => InCompute(head.pos, term)
    }
  def instruction: Parser[Instruction[Position]] = inAssume | inDefine | inPrint | inCompute

  def instructions: Parser[List[Instruction[Position]]] = phrase(rep(instruction))
}
