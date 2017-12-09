package pts

import scala.collection.immutable.Set

sealed abstract class Term[I] {
  val freeVars: Set[String]
  def renameFreeVar(oldName: String, newName: String): Term[I]
  def alphaEquals(term: Term[I]): Boolean
  def substitute(name: String, term: Term[I]): Term[I]
}

// variable
case class TmVar[I](info: I, name: String) extends Term[I] {
  val freeVars: Set[String] = Set(this.name)

  override def toString(): String = this.name

  def renameFreeVar(oldName: String, newName: String): TmVar[I] =
    if (this.name == oldName)
      TmVar(this.info, newName)
    else
      this

  def alphaEquals(term: Term[I]): Boolean = term match {
    case TmVar(_, name) => this.name == name
    case _ => false
  }

  def substitute(name: String, term: Term[I]): Term[I] =
    if (this.name == name)
      term
    else
      this
}

// constant
case class TmConst[I](info: I, name: String) extends Term[I] {
  val freeVars: Set[String] = Set.empty

  override def toString(): String = this.name

  def renameFreeVar(oldName: String, newName: String): TmConst[I] = this

  def alphaEquals(term: Term[I]): Boolean = term match {
    case TmConst(_, name) => this.name == name
    case _ => false
  }

  def substitute(name: String, term: Term[I]): Term[I] = this
}

// application
case class TmApp[I](info: I, func: Term[I], arg: Term[I]) extends Term[I] {
  val freeVars: Set[String] = this.func.freeVars | this.arg.freeVars

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

  def renameFreeVar(oldName: String, newName: String): TmApp[I] = {
    val newFunc = this.func.renameFreeVar(oldName, newName)
    val newArg = this.arg.renameFreeVar(oldName, newName)
    TmApp(this.info, newFunc, newArg)
  }

  def alphaEquals(term: Term[I]): Boolean = term match {
    case TmApp(_, func, arg) => this.func.alphaEquals(func) && this.arg.alphaEquals(arg)
    case _ => false
  }

  def substitute(name: String, term: Term[I]): Term[I] =
    TmApp(this.info, this.func.substitute(name, term), this.arg.substitute(name, term))
}

// abstraction
case class TmAbs[I](info: I, paramName: String, paramType: Term[I], body: Term[I]) extends Term[I] {
  val freeVars: Set[String] = this.paramType.freeVars | (this.body.freeVars - this.paramName)

  override def toString(): String =
    "fun " + this.paramName +
    ": " + this.paramType.toString +
    ". " + this.body.toString

  def renameFreeVar(oldName: String, newName: String): TmAbs[I] = {
    val newParamType = this.paramType.renameFreeVar(oldName, newName)
    val newBody =
      if (this.paramName == oldName)
        this.body
      else
        this.body.renameFreeVar(oldName, newName)
    TmAbs(this.info, this.paramName, newParamType, newBody)
  }

  def alphaEquals(term: Term[I]): Boolean = term match {
    case TmAbs(_, paramName, paramType, body) => {
      if (this.paramType.alphaEquals(paramType)) {
        val usedNames = (this.body.freeVars - this.paramName) | (body.freeVars - paramName)
        val newParamName = Util.getFreshVarName("_", usedNames)
        this.body.renameFreeVar(this.paramName, newParamName)
          .alphaEquals(body.renameFreeVar(paramName, newParamName))
      }
      else
        false
    }
    case _ => false
  }

  def substitute(name: String, term: Term[I]): Term[I] = {
    val newParamType = this.paramType.substitute(name, term)
    // if substituion will occur in the body
    if (this.paramName != name && this.body.freeVars.contains(name)) {
      // if alpha-conversion is needed
      if (term.freeVars.contains(this.paramName)) {
        val usedNames = term.freeVars | this.body.freeVars
        val newParamName = Util.getFreshVarName("_", usedNames)
        val newBody = this.body.renameFreeVar(this.paramName, newParamName)
          .substitute(name, term)
        TmAbs(this.info, newParamName, newParamType, newBody)
      }
      else
        TmAbs(this.info, this.paramName, newParamType, this.body.substitute(name, term))
    }
    else
      TmAbs(this.info, this.paramName, newParamType, this.body)
  }
}

// product
case class TmProd[I](info: I, paramName: String, paramType: Term[I], body: Term[I]) extends Term[I] {
  val freeVars: Set[String] = this.paramType.freeVars | (this.body.freeVars - this.paramName)

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

  def renameFreeVar(oldName: String, newName: String): TmProd[I] = {
    val newParamType = this.paramType.renameFreeVar(oldName, newName)
    val newBody =
      if (this.paramName == oldName)
        this.body
      else
        this.body.renameFreeVar(oldName, newName)
    TmProd(this.info, this.paramName, newParamType, newBody)
  }

  def alphaEquals(term: Term[I]): Boolean = term match {
    case TmProd(_, paramName, paramType, body) => {
      if (this.paramType.alphaEquals(paramType)) {
        val usedNames = (this.body.freeVars - this.paramName) | (body.freeVars - paramName)
        val newParamName = Util.getFreshVarName("_", usedNames)
        this.body.renameFreeVar(this.paramName, newParamName)
          .alphaEquals(body.renameFreeVar(paramName, newParamName))
      }
      else
        false
    }
    case _ => false
  }

  def substitute(name: String, term: Term[I]): Term[I] = {
    val newParamType = this.paramType.substitute(name, term)
    // if substituion will occur in the body
    if (this.paramName != name && this.body.freeVars.contains(name)) {
      // if alpha-conversion is needed
      if (term.freeVars.contains(this.paramName)) {
        val usedNames = term.freeVars | this.body.freeVars
        val newParamName = Util.getFreshVarName("_", usedNames)
        val newBody = this.body.renameFreeVar(this.paramName, newParamName)
          .substitute(name, term)
        TmProd(this.info, newParamName, newParamType, newBody)
      }
      else
        TmProd(this.info, this.paramName, newParamType, this.body.substitute(name, term))
    }
    else
      TmProd(this.info, this.paramName, newParamType, this.body)
  }
}
