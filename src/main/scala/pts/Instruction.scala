package pts

sealed abstract class Instruction[+I] {
  val info: I
  def exec[J >: I](env: Term.Env[J]): Either[String, Term.Env[J]]
}

case class InAssume[+I](info: I, name: String, itsType: Term[I]) extends Instruction[I] {
  def exec[J >: I](env: Term.Env[J]): Either[String, Term.Env[J]] = ???
}

case class InDefine[+I](info: I, name: String, itsType: Option[Term[I]], term: Term[I]) extends Instruction[I] {
  def exec[J >: I](env: Term.Env[J]): Either[String, Term.Env[J]] = ???
}

case class InPrint[+I](info: I, name: String) extends Instruction[I] {
  def exec[J >: I](env: Term.Env[J]): Either[String, Term.Env[J]] = ???
}

case class InCompute[+I](info: I, term: Term[I]) extends Instruction[I] {
  def exec[J >: I](env: Term.Env[J]): Either[String, Term.Env[J]] = ???
}
