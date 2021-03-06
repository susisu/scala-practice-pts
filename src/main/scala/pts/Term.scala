package pts

import scala.collection.immutable._

sealed abstract class Term[+I] {
  val info: I
  val freeVars: Set[String]
  def isConstant: Boolean = false
  def isProduct: Boolean = false
  def renameFreeVar(oldName: String, newName: String): Term[I]
  def alphaEquals[J](term: Term[J]): Boolean
  def substitute[J >: I](name: String, term: Term[J]): Term[J]
}

// variable
case class TmVar[+I](info: I, name: String) extends Term[I] {
  override def toString(): String = this.name

  lazy val freeVars = Set(this.name)

  def renameFreeVar(oldName: String, newName: String): TmVar[I] =
    if (this.name == oldName)
      TmVar(this.info, newName)
    else
      this

  def alphaEquals[J](term: Term[J]): Boolean = term match {
    case TmVar(_, name) => this.name == name
    case _ => false
  }

  def substitute[J >: I](name: String, term: Term[J]): Term[J] =
    if (this.name == name)
      term
    else
      this
}

// constant
case class TmConst[+I](info: I, name: String) extends Term[I] {
  override def toString(): String = this.name

  lazy val freeVars = Set.empty

  override def isConstant = true

  def renameFreeVar(oldName: String, newName: String): TmConst[I] = this

  def alphaEquals[J](term: Term[J]): Boolean = term match {
    case TmConst(_, name) => this.name == name
    case _ => false
  }

  def substitute[J >: I](name: String, term: Term[J]): Term[J] = this
}

// application
case class TmApp[+I](info: I, func: Term[I], arg: Term[I]) extends Term[I] {
  override def toString(): String = {
    val funcStr = this.func match {
      case TmVar(_, _) | TmConst(_, _) | TmApp(_, _, _) => this.func.toString
      case _ => "(" + this.func.toString + ")"
    }
    val argStr = this.arg match {
      case TmVar(_, _) | TmConst(_, _) => this.arg.toString
      case _ => "(" + this.arg.toString + ")"
    }
    funcStr + " " + argStr
  }

  lazy val freeVars = this.func.freeVars | this.arg.freeVars

  def renameFreeVar(oldName: String, newName: String): TmApp[I] = {
    val _func = this.func.renameFreeVar(oldName, newName)
    val _arg = this.arg.renameFreeVar(oldName, newName)
    TmApp(this.info, _func, _arg)
  }

  def alphaEquals[J](term: Term[J]): Boolean = term match {
    case TmApp(_, func, arg) => this.func.alphaEquals(func) && this.arg.alphaEquals(arg)
    case _ => false
  }

  def substitute[J >: I](name: String, term: Term[J]): Term[J] =
    TmApp(this.info, this.func.substitute(name, term), this.arg.substitute(name, term))
}

// abstraction
case class TmAbs[+I](info: I, paramName: String, paramType: Term[I], body: Term[I]) extends Term[I] {
  override def toString(): String =
    "fun " + this.paramName +
    ": " + this.paramType.toString +
    ". " + this.body.toString

  lazy val freeVars = this.paramType.freeVars | (this.body.freeVars - this.paramName)

  def renameFreeVar(oldName: String, newName: String): TmAbs[I] = {
    val _paramType = this.paramType.renameFreeVar(oldName, newName)
    // if rename will occur in the body
    if (this.paramName != oldName && this.body.freeVars.contains(oldName)) {
      // if alpha-conversion is needed
      if (this.paramName == newName) {
        val usedNames = this.body.freeVars + newName
        val _paramName = Util.getFreshVarName("_", usedNames)
        val _body = this.body.renameFreeVar(this.paramName, _paramName)
          .renameFreeVar(oldName, newName)
        TmAbs(this.info, _paramName, _paramType, _body)
      }
      else
        TmAbs(this.info, this.paramName, _paramType, this.body.renameFreeVar(oldName, newName))
    }
    else
      TmAbs(this.info, this.paramName, _paramType, this.body)
  }

  def alphaEquals[J](term: Term[J]): Boolean = term match {
    case TmAbs(_, paramName, paramType, body) => {
      if (this.paramType.alphaEquals(paramType)) {
        val usedNames = (this.body.freeVars - this.paramName) | (body.freeVars - paramName)
        val _paramName = Util.getFreshVarName("_", usedNames)
        this.body.renameFreeVar(this.paramName, _paramName)
          .alphaEquals(body.renameFreeVar(paramName, _paramName))
      }
      else
        false
    }
    case _ => false
  }

  def substitute[J >: I](name: String, term: Term[J]): Term[J] = {
    val _paramType = this.paramType.substitute(name, term)
    // if substituion will occur in the body
    if (this.paramName != name && this.body.freeVars.contains(name)) {
      // if alpha-conversion is needed
      if (term.freeVars.contains(this.paramName)) {
        val usedNames = term.freeVars | this.body.freeVars
        val _paramName = Util.getFreshVarName("_", usedNames)
        val _body = this.body.renameFreeVar(this.paramName, _paramName)
          .substitute(name, term)
        TmAbs(this.info, _paramName, _paramType, _body)
      }
      else
        TmAbs(this.info, this.paramName, _paramType, this.body.substitute(name, term))
    }
    else
      TmAbs(this.info, this.paramName, _paramType, this.body)
  }
}

// product
case class TmProd[+I](info: I, paramName: String, paramType: Term[I], body: Term[I]) extends Term[I] {
  override def toString(): String =
    if (!this.body.freeVars.contains(paramName)) {
      val domStr = this.paramType match {
        case TmVar(_, _) | TmConst(_, _) | TmApp(_, _, _) => this.paramType.toString
        case _ => "(" + this.paramType.toString + ")"
      }
      domStr + " -> " + this.body.toString
    }
    else "forall " + this.paramName +
      ": " + this.paramType.toString +
      ". " + this.body.toString

  lazy val freeVars = this.paramType.freeVars | (this.body.freeVars - this.paramName)

  override def isProduct = true

  def renameFreeVar(oldName: String, newName: String): TmProd[I] = {
    val _paramType = this.paramType.renameFreeVar(oldName, newName)
    // if rename will occur in the body
    if (this.paramName != oldName && this.body.freeVars.contains(oldName)) {
      // if alpha-conversion is needed
      if (this.paramName == newName) {
        val usedNames = this.body.freeVars + newName
        val _paramName = Util.getFreshVarName(this.paramName, usedNames)
        val _body = this.body.renameFreeVar(this.paramName, _paramName)
          .renameFreeVar(oldName, newName)
        TmProd(this.info, _paramName, _paramType, _body)
      }
      else
        TmProd(this.info, this.paramName, _paramType, this.body.renameFreeVar(oldName, newName))
    }
    else
      TmProd(this.info, this.paramName, _paramType, this.body)
  }

  def alphaEquals[J](term: Term[J]): Boolean = term match {
    case TmProd(_, paramName, paramType, body) => {
      if (this.paramType.alphaEquals(paramType)) {
        val usedNames = (this.body.freeVars - this.paramName) | (body.freeVars - paramName)
        val _paramName = Util.getFreshVarName(this.paramName, usedNames)
        this.body.renameFreeVar(this.paramName, _paramName)
          .alphaEquals(body.renameFreeVar(paramName, _paramName))
      }
      else
        false
    }
    case _ => false
  }

  def substitute[J >: I](name: String, term: Term[J]): Term[J] = {
    val _paramType = this.paramType.substitute(name, term)
    // if substituion will occur in the body
    if (this.paramName != name && this.body.freeVars.contains(name)) {
      // if alpha-conversion is needed
      if (term.freeVars.contains(this.paramName)) {
        val usedNames = term.freeVars | this.body.freeVars
        val _paramName = Util.getFreshVarName(this.paramName, usedNames)
        val _body = this.body.renameFreeVar(this.paramName, _paramName)
          .substitute(name, term)
        TmProd(this.info, _paramName, _paramType, _body)
      }
      else
        TmProd(this.info, this.paramName, _paramType, this.body.substitute(name, term))
    }
    else
      TmProd(this.info, this.paramName, _paramType, this.body)
  }
}

object Term {
  type Env[I] = Map[String, (Term[I], Option[Term[I]])]

  def normalize[I](env: Env[I], term: Term[I])(implicit si: SourceInfo[I]): Either[String, Term[I]] = term match {
    case TmVar(info, name) =>
      env.get(name) match {
        case Some((_, Some(_term))) => Term.normalize(env, _term)
        case Some((_, None)) => Right(term)
        case None => Left(si.showMessage(info, s"`$name` is not declared"))
      }
    case TmConst(_, _) => Right(term)
    case TmApp(info, func, arg) =>
      Term.normalize(env, func).flatMap { _func =>
        Term.normalize(env, arg).flatMap { _arg =>
          _func match {
            case TmAbs(_, paramName, _, body) => {
              val _body = body.substitute(paramName, _arg)
              Term.normalize(env, _body)
            }
            case _ => Right(TmApp(info, _func, _arg))
          }
        }
      }
    case TmAbs(info, paramName, paramType, body) =>
      Term.normalize(env, paramType).flatMap { _paramType =>
        if (env.contains(paramName)) {
          val _paramName = Util.getFreshVarName(paramName, env.keySet)
          val _env = env + (_paramName -> ((_paramType, None)))
          Term.normalize(_env, body.renameFreeVar(paramName, _paramName)).map {
            TmAbs(info, _paramName, _paramType, _)
          }
        }
        else {
          val _env = env + (paramName -> ((_paramType, None)))
          Term.normalize(_env, body).map {
            TmAbs(info, paramName, _paramType, _)
          }
        }
      }
    case TmProd(info, paramName, paramType, body) =>
      Term.normalize(env, paramType).flatMap { _paramType =>
        if (env.contains(paramName)) {
          val _paramName = Util.getFreshVarName(paramName, env.keySet)
          val _env = env + (_paramName -> ((_paramType, None)))
          Term.normalize(_env, body.renameFreeVar(paramName, _paramName)).map {
            TmProd(info, _paramName, _paramType, _)
          }
        }
        else {
          val _env = env + (paramName -> ((_paramType, None)))
          Term.normalize(_env, body).map {
            TmProd(info, paramName, _paramType, _)
          }
        }
      }
  }

  def weakNormalize[I](env: Env[I], term: Term[I])(implicit si: SourceInfo[I]): Either[String, Term[I]] = term match {
    case TmVar(info, name) =>
      env.get(name) match {
        case Some((_, Some(_term))) => Term.weakNormalize(env, _term)
        case Some((_, None)) => Right(term)
        case None => Left(si.showMessage(info, s"`$name` is not declared"))
      }
    case TmConst(_, _) => Right(term)
    case TmApp(info, func, arg) =>
      Term.weakNormalize(env, func).flatMap {
        case TmAbs(_, paramName, _, body) => {
          val _body = body.substitute(paramName, arg)
          Term.weakNormalize(env, _body)
        }
        case _func => Right(TmApp(info, _func, arg))
      }
    case TmAbs(_, _, _, _) | TmProd(_, _, _, _) => Right(term)
  }
}
