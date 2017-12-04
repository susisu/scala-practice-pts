package pts

import scala.collection.immutable.Set

sealed abstract class Term[I] {
  def getInfo(): I
  def hasFreeVar(name: String): Boolean
  def getFreeVars(): Set[String]
}

// variable
case class TmVar[I](info: I, name: String) extends Term[I] {
  override def toString(): String = this.name

  def getInfo(): I = this.info

  def hasFreeVar(name: String): Boolean = this.name == name

  def getFreeVars(): Set[String] = Set(this.name)
}

// constant
case class TmConst[I](info: I, name: String) extends Term[I] {
  override def toString(): String = this.name

  def getInfo(): I = this.info

  def hasFreeVar(name: String): Boolean = false

  def getFreeVars(): Set[String] = Set.empty
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

  def getInfo(): I = this.info

  def hasFreeVar(name: String): Boolean = this.func.hasFreeVar(name) || this.arg.hasFreeVar(name)

  def getFreeVars(): Set[String] = this.func.getFreeVars | this.arg.getFreeVars
}

// abstraction
case class TmAbs[I](info: I, paramName: String, paramType: Term[I], body: Term[I]) extends Term[I] {
  override def toString(): String =
    "fun " + this.paramName +
    ": " + this.paramType.toString +
    ". " + this.body.toString

  def getInfo(): I = this.info

  def hasFreeVar(name: String): Boolean =
    if (this.paramName == name)
      this.paramType.hasFreeVar(name)
    else
      this.paramType.hasFreeVar(name) || this.body.hasFreeVar(name)

  def getFreeVars(): Set[String] = this.paramType.getFreeVars | (this.body.getFreeVars - this.paramName)
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

  def getInfo(): I = this.info

  def hasFreeVar(name: String): Boolean =
    if (this.paramName == name)
      this.paramType.hasFreeVar(name)
    else
      this.paramType.hasFreeVar(name) || this.body.hasFreeVar(name)

  def getFreeVars(): Set[String] = this.paramType.getFreeVars | (this.body.getFreeVars - this.paramName)
}
