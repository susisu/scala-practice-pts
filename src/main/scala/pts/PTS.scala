package pts

case class PTS(sorts: PTS.Sorts, axioms: PTS.Axioms, rules: PTS.Rules) {
  def typeOf[I](env: Term.Env[Option[I]], term: Term[Option[I]]): Either[String, Term[Option[I]]] =
    term match {
      case TmVar(info, name) =>
        env.get(name) match {
          case Some((_term, _)) => Right(_term)
          case _ => PTS.typeError(info, s"variable `$name` is not declared")
        }
      case TmConst(info, name) =>
        this.axioms.get(name) match {
          case Some(sort) => Right(TmConst(None, sort))
          case None => PTS.typeError(info, s"no axiom for constant `$name`")
        }
      case TmApp(info, func, arg) =>
        for {
          funcType <- this.typeOf(env, func);
          _funcType = Term.weakNormalize(env, funcType);
          ret <- _funcType match {
            case TmProd(_, paramName, paramType, body) =>
              for {
                argType <- this.typeOf(env, arg);
                _argType = Term.normalize(env, argType)
                _paramType = Term.normalize(env, paramType)
                ret <-
                  if (argType.alphaEquals(_paramType))
                    Right(body.substitute(paramName, arg))
                  else
                    PTS.typeError(
                      arg.info,
                      s"argument `${arg.toString}` has type `${argType.toString}`" +
                      s" which does not match parameter type `${paramType.toString}`"
                    )
              } yield ret
            case _ =>
              PTS.typeError(
                func.info,
                s"function `${func.toString}` has type `${funcType.toString}`" +
                " which is not a product"
              )
          }
        } yield ret
      case TmAbs(info, paramName, paramType, body) =>
        for {
          termType <-
            if (env.contains(paramName)) {
              val _paramName = Util.getFreshVarName("_", env.keySet)
              val _body = body.renameFreeVar(paramName, _paramName)
              val _env = env + (_paramName -> ((paramType, None)))
              for {
                bodyType <- this.typeOf(_env, _body)
              } yield TmProd(None, _paramName, paramType, bodyType)
            }
            else {
              val _env = env + (paramName -> ((paramType, None)))
              for {
                bodyType <- this.typeOf(_env, body)
              } yield TmProd(None, paramName, paramType, bodyType)
            };
          termTypeType <- this.typeOf(env, termType);
          _termTypeType = Term.weakNormalize(env, termTypeType);
          ret <- _termTypeType match {
            case TmConst(_, sort) =>
              if (this.sorts.contains(sort))
                Right(termType)
              else
                PTS.typeError(
                  info,
                  s"abstraction `${term.toString}` has type `${termType.toString}`" +
                  s" which has type `${termTypeType.toString}` which is not a sort"
                )
            case _ =>
              PTS.typeError(
                info,
                s"abstraction `${term.toString}` has type `${termType.toString}`" +
                s" which has type `${termTypeType.toString}` which is not a sort"
              )
          }
        } yield ret
      case TmProd(info, paramName, paramType, body) =>
        for {
          paramTypeType <- this.typeOf(env, paramType);
          _paramTypeType = Term.weakNormalize(env, paramTypeType);
          ret <- _paramTypeType match {
            case TmConst(_, sort1) =>
              if (this.sorts.contains(sort1)) {
                val (_env, _body) =
                  if (env.contains(paramName)) {
                    val _paramName = Util.getFreshVarName("_", env.keySet)
                    val _body = body.renameFreeVar(paramName, _paramName)
                    val _env = env + (_paramName -> ((paramType, None)))
                    (_env, _body)
                  }
                  else {
                    val _env = env + (paramName -> ((paramType, None)))
                    (_env, body)
                  }
                for {
                  bodyType <- this.typeOf(_env, _body);
                  _bodyType = Term.weakNormalize(_env, bodyType);
                  ret <- _bodyType match {
                    case TmConst(_, sort2) =>
                      if (this.sorts.contains(sort2))
                        this.rules.get((sort1, sort2)) match {
                          case Some(sort3) => Right(TmConst(None, sort3))
                          case None => PTS.typeError(info, s"no rule for sorts `$sort1`, `$sort2`")
                        }
                      else
                        PTS.typeError(
                          body.info,
                          s"body ${body.toString} has `${bodyType.toString}`" +
                          " which is not a sort"
                        )
                    case _ =>
                      PTS.typeError(
                        body.info,
                        s"body ${body.toString} has `${bodyType.toString}`" +
                        " which is not a sort"
                      )
                  }
                } yield ret
              }
              else
                PTS.typeError(
                  paramType.info,
                  s"parameter type `${paramType.toString}` has type `${paramTypeType.toString}`" +
                  " which is not a sort"
                )
            case _ =>
              PTS.typeError(
                paramType.info,
                s"parameter type `${paramType.toString}` has type `${paramTypeType.toString}`" +
                " which is not a sort"
              )
          }
        } yield ret
    }
}

object PTS {
  type Sorts = Set[String]
  type Axioms = Map[String, String]
  type Rules = Map[(String, String), String]

  private def typeError[I, T](info: Option[I], message: String): Either[String, T] = {
    val prefix = info match {
      case Some(i) => i.toString + ": "
      case None => ""
    }
    Left(prefix + message)
  }
}
