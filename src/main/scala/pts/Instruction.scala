package pts

import scalaz.std.either._

sealed abstract class Instruction[+I] {
  val info: I
  def exec[J >: I](pts: PTS, env: Term.Env[J])(implicit si: SourceInfo[J]): Either[String, (String, Term.Env[J])]
}

case class InAssume[+I](info: I, name: String, itsType: Term[I]) extends Instruction[I] {
  def exec[J >: I](pts: PTS, env: Term.Env[J])(implicit si: SourceInfo[J]): Either[String, (String, Term.Env[J])] =
    env.get(this.name) match {
      case Some(_) => Left(si.showMessage(this.info, s"`$name` is already declared"))
      case None => for {
        itsTypeType <- pts.typeOf(env, this.itsType)
        _ <- eitherMonad.whenM(!itsTypeType.isConstant) {
          Left(si.showMessage(this.info, s"`${itsType.toString}` is not a type"))
        }
        TmConst(_, sort) = itsTypeType
        _ <- eitherMonad.whenM(!pts.sorts.contains(sort)) {
          Left(si.showMessage(this.info, s"`${itsType.toString}` is not a type"))
        }
      } yield (s"$name: ${this.itsType.toString}", env + (this.name -> ((this.itsType, None))))
    }
}

case class InDefine[+I](info: I, name: String, itsType: Option[Term[I]], term: Term[I]) extends Instruction[I] {
  def exec[J >: I](pts: PTS, env: Term.Env[J])(implicit si: SourceInfo[J]): Either[String, (String, Term.Env[J])] =
    env.get(this.name) match {
      case Some(_) => Left(si.showMessage(this.info, s"`$name` is already declared"))
      case None => this.itsType match {
        case None => for {
            termType <- pts.typeOf(env, this.term)
          } yield (s"$name: ${termType.toString}", env + (this.name -> ((termType, Some(this.term)))))
        case Some(itsType) => for {
          termType <- pts.typeOf(env, term);
          _ <- pts.typeOf(env, itsType);
          _termType <- Term.normalize(env, termType);
          _itsType <- Term.normalize(env, itsType);
          _ <- eitherMonad.whenM(!_termType.alphaEquals(_itsType)) {
            Left(si.showMessage(
              this.info,
              s"`${term.toString}` has type `${itsType.toString}` which does not match the expected type"
            ))
          }
        } yield (s"$name: ${itsType.toString}", env + (this.name -> ((itsType, Some(this.term)))))
      }
    }
}

case class InPrint[+I](info: I, name: String) extends Instruction[I] {
  def exec[J >: I](pts: PTS, env: Term.Env[J])(implicit si: SourceInfo[J]): Either[String, (String, Term.Env[J])] =
    env.get(this.name) match {
      case None => Left(s"`$name` is not declared")
      case Some((itsType, None)) => Right((s"$name: ${itsType.toString}", env))
      case Some((itsType, Some(term))) => Right((s"$name: ${itsType.toString}\n= ${term.toString}", env))
    }
}

case class InCompute[+I](info: I, term: Term[I]) extends Instruction[I] {
  def exec[J >: I](pts: PTS, env: Term.Env[J])(implicit si: SourceInfo[J]): Either[String, (String, Term.Env[J])] =
    for {
      _ <- pts.typeOf(env, term);
      _term <- Term.normalize(env, term)
    } yield (s"-> ${_term.toString}", env)
}
