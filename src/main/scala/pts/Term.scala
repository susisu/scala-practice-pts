package pts

import scala.collection.immutable.Set

sealed abstract class Term[I] {
  def hasFreeVar(name: String): Boolean
  def freeVars(): Set[String]
  def renameFreeVar(oldName: String, newName: String): Term[I]
}

// variable
case class TmVar[I](info: I, name: String) extends Term[I] {
  override def toString(): String = this.name

  def hasFreeVar(name: String): Boolean = this.name == name

  def freeVars(): Set[String] = Set(this.name)

  def renameFreeVar(oldName: String, newName: String): TmVar[I] =
    if (this.name == oldName)
      TmVar(this.info, newName)
    else
      this
}

// constant
case class TmConst[I](info: I, name: String) extends Term[I] {
  override def toString(): String = this.name

  def hasFreeVar(name: String): Boolean = false

  def freeVars(): Set[String] = Set.empty

  def renameFreeVar(oldName: String, newName: String): TmConst[I] = this
}

// application
case class TmApp[I](info: I, func: Term[I], arg: Term[I]) extends Term[I] {
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

  def hasFreeVar(name: String): Boolean = this.func.hasFreeVar(name) || this.arg.hasFreeVar(name)

  def freeVars(): Set[String] = this.func.freeVars | this.arg.freeVars

  def renameFreeVar(oldName: String, newName: String): TmApp[I] = {
    val newFunc = this.func.renameFreeVar(oldName, newName)
    val newArg = this.arg.renameFreeVar(oldName, newName)
    TmApp(this.info, newFunc, newArg)
  }
}

// abstraction
case class TmAbs[I](info: I, paramName: String, paramType: Term[I], body: Term[I]) extends Term[I] {
  override def toString(): String =
    "fun " + this.paramName +
    ": " + this.paramType.toString +
    ". " + this.body.toString

  def hasFreeVar(name: String): Boolean =
    if (this.paramName == name)
      this.paramType.hasFreeVar(name)
    else
      this.paramType.hasFreeVar(name) || this.body.hasFreeVar(name)

  def freeVars(): Set[String] = this.paramType.freeVars | (this.body.freeVars - this.paramName)

  def renameFreeVar(oldName: String, newName: String): TmAbs[I] = {
    val newParamType = this.paramType.renameFreeVar(oldName, newName)
    val newBody =
      if (this.paramName == oldName)
        this.body
      else
        this.body.renameFreeVar(oldName, newName)
    TmAbs(this.info, this.paramName, newParamType, newBody)
  }
}

// product
case class TmProd[I](info: I, paramName: String, paramType: Term[I], body: Term[I]) extends Term[I] {
  override def toString(): String =
    if (!this.body.hasFreeVar(paramName)) {
      val domStr = this.paramType match {
        case TmVar(_, _) | TmConst(_, _) | TmApp(_, _, _) => this.paramType.toString
        case _ => "(" + this.paramType.toString + ")"
      }
      domStr + " -> " + this.body.toString
    }
    else "forall " + this.paramName +
      ": " + this.paramType.toString +
      ". " + this.body.toString

  def hasFreeVar(name: String): Boolean =
    if (this.paramName == name)
      this.paramType.hasFreeVar(name)
    else
      this.paramType.hasFreeVar(name) || this.body.hasFreeVar(name)

  def freeVars(): Set[String] = this.paramType.freeVars | (this.body.freeVars - this.paramName)

  def renameFreeVar(oldName: String, newName: String): TmProd[I] = {
    val newParamType = this.paramType.renameFreeVar(oldName, newName)
    val newBody =
      if (this.paramName == oldName)
        this.body
      else
        this.body.renameFreeVar(oldName, newName)
    TmProd(this.info, this.paramName, newParamType, newBody)
  }
}
