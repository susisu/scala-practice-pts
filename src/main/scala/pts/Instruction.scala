package pts

sealed abstract class Instruction[+I] {
  val info: I
  def exec[J >: I](pts: PTS, env: Term.Env[J])(implicit si: SourceInfo[J]): (String, Term.Env[J])
}

case class InAssume[+I](info: I, name: String, itsType: Term[I]) extends Instruction[I] {
  def exec[J >: I](pts: PTS, env: Term.Env[J])(implicit si: SourceInfo[J]): (String, Term.Env[J]) =
    env.get(this.name) match {
      case Some(_) =>
        (
          si.showMessage(this.info, s"`$name` is already declared"),
          env
        )
      case None =>
        pts.typeOf(env, this.itsType) match {
          case Left(err) => (err, env)
          case Right(_) =>
            (s"$name: ${this.itsType.toString}", env + (this.name -> ((this.itsType, None))))
        }
    }
}

case class InDefine[+I](info: I, name: String, itsType: Option[Term[I]], term: Term[I]) extends Instruction[I] {
  def exec[J >: I](pts: PTS, env: Term.Env[J])(implicit si: SourceInfo[J]): (String, Term.Env[J]) =
    env.get(this.name) match {
      case Some(_) =>
        (
          si.showMessage(this.info, s"`$name` is already declared"),
          env
        )
      case None =>
        pts.typeOf(env, term) match {
          case Left(err) => (err, env)
          case Right(termType) =>
            this.itsType match {
              case None =>
                (s"$name: ${termType.toString}", env + (this.name -> ((termType, Some(this.term)))))
              case Some(itsType) =>
                pts.typeOf(env, itsType) match {
                  case Left(err) => (err, env)
                  case Right(_) => {
                    val typesMatch = for {
                      _itsType <- Term.normalize(env, itsType);
                      _termType <- Term.normalize(env, termType)
                    } yield _itsType.alphaEquals(_termType)
                    typesMatch match {
                      case Left(err) => (err, env)
                      case Right(false) =>
                        (
                          si.showMessage(
                            this.info,
                            s"`$name` is expected to have type `${itsType.toString}`" +
                            s"but it actually has `${termType.toString}`"
                          ),
                          env
                        )
                      case Right(true) =>
                        (s"$name: ${itsType.toString}", env + (this.name -> ((itsType, Some(this.term)))))
                    }
                  }
                }
            }
        }
    }
}

case class InPrint[+I](info: I, name: String) extends Instruction[I] {
  def exec[J >: I](pts: PTS, env: Term.Env[J])(implicit si: SourceInfo[J]): (String, Term.Env[J]) =
    env.get(this.name) match {
      case None => (s"`$name` is not declared", env)
      case Some((itsType, None)) => (s"$name: ${itsType.toString}", env)
      case Some((itsType, Some(term))) => (s"$name: ${itsType.toString}\n= ${term.toString}", env)
    }
}

case class InCompute[+I](info: I, term: Term[I]) extends Instruction[I] {
  def exec[J >: I](pts: PTS, env: Term.Env[J])(implicit si: SourceInfo[J]): (String, Term.Env[J]) = {
    val _termOpt = for {
      itsType <- pts.typeOf(env, term); // assert the computation will terminate
      _term <- Term.normalize(env, term)
    } yield _term
    _termOpt match {
      case Left(err) => (err, env)
      case Right(_term) => (s"-> ${_term.toString}", env)
    }
  }
}
