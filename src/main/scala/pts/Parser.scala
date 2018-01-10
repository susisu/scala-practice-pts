package pts

import scala.util.parsing.combinator._
import scala.util.parsing.input._

object Parser extends RegexParsers {
  sealed abstract trait Token extends Positional
  case class TkLParen() extends Token
  case class TkRParen() extends Token
  case class TkFun() extends Token
  case class TkForall() extends Token
  case class TkComma() extends Token
  case class TkDot() extends Token
  case class TkColon() extends Token
  case class TkArrow() extends Token
  case class TkUnder() extends Token
  case class TkIdent(name: String) extends Token
  case class TkConst(name: String) extends Token
  case class TkAssume() extends Token
  case class TkDefine() extends Token
  case class TkPrint() extends Token
  case class TkReduce() extends Token
  case class TkEqual() extends Token
  case class TkSemi() extends Token

  override def skipWhitespace = false

  def whitespace: Parser[Unit] = "[ \t\r\n\f]*".r ^^^ (())
  def lex[T](p: Parser[T]): Parser[T] = p <~ whitespace
  def word[T](p: Parser[T]): Parser[T] = lex(p <~ not("[A-Za-z0-9_]".r))
  def op[T](p: Parser[T]): Parser[T] = lex(p <~ not(".:->=".r))

  val reservedNames: Set[String] = Set("fun", "forall")

  def tkLParen: Parser[TkLParen] = positioned(lex("(")       ^^ { _ => TkLParen() })
  def tkRParen: Parser[TkRParen] = positioned(lex(")")       ^^ { _ => TkRParen() })
  def tkFun   : Parser[TkFun]    = positioned(word("fun")    ^^ { _ => TkFun() })
  def tkForall: Parser[TkFun]    = positioned(word("forall") ^^ { _ => TkFun() })
  def tkComma : Parser[TkComma]  = positioned(op(",")        ^^ { _ => TkComma() })
  def tkDot   : Parser[TkDot]    = positioned(op(".")        ^^ { _ => TkDot() })
  def tkColon : Parser[TkColon]  = positioned(op(":")        ^^ { _ => TkColon() })
  def tkArrow : Parser[TkArrow]  = positioned(op("->")       ^^ { _ => TkArrow() })
  def tkUnder : Parser[TkUnder]  = positioned(op("_")        ^^ { _ => TkUnder() })

  def tkIdent: Parser[TkIdent] = positioned(
    lex("[A-Za-z][A-Za-z0-9_]*".r.filter { !reservedNames.contains(_) }) ^^ { TkIdent(_) }
  )
  def tkConst: Parser[TkConst] = positioned(lex(raw"[\*#]+".r) ^^ { TkConst(_) })

  def tkAssume: Parser[TkAssume] = positioned(word("assume") ^^ { _ => TkAssume() })
  def tkDefine: Parser[TkDefine] = positioned(word("define") ^^ { _ => TkDefine() })
  def tkPrint : Parser[TkPrint]  = positioned(word("print")  ^^ { _ => TkPrint() })
  def tkReduce: Parser[TkReduce] = positioned(word("reduce") ^^ { _ => TkReduce() })
  def tkEqual : Parser[TkEqual]  = positioned(op("=")        ^^ { _ => TkEqual() })
  def tkSemi  : Parser[TkSemi]   = positioned(lex(";")       ^^ { _ => TkSemi() })

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
  def pattern: Parser[String] =
      tkUnder ^^^ "_" |
      tkIdent ^^ { _.name }
  def binding: Parser[String ~ Term[Position]] = pattern ~ (tkColon ~> term)
  def tmAbs: Parser[Term[Position]] =
    tkFun ~ rep1sep(binding, tkComma) ~ (tkDot ~> term) ^^ {
      case head ~ bindings ~ body =>
        bindings.foldRight(body) {
          case (paramName ~ paramType, rest) => TmAbs(head.pos, paramName, paramType, rest)
        }
    }
  def tmProd: Parser[Term[Position]] =
    tkForall ~ rep1sep(binding, tkComma) ~ (tkDot ~> term) ^^ {
      case head ~ bindings ~ body =>
        bindings.foldRight(body) {
          case (paramName ~ paramType, rest) => TmProd(head.pos, paramName, paramType, rest)
        }
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
  def inReduce: Parser[InReduce[Position]] =
    tkReduce ~ term ^^ {
      case head ~ term => InReduce(head.pos, term)
    }
  def instruction: Parser[Instruction[Position]] = inAssume | inDefine | inPrint | inReduce

  def instructions: Parser[List[Instruction[Position]]] =
    phrase(whitespace ~> rep(instruction <~ tkSemi) <~ whitespace)
}
