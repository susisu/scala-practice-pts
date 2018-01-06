package pts

import scalaz.std.either._

case class PTS(sorts: PTS.Sorts, axioms: PTS.Axioms, rules: PTS.Rules) {
  def typeOf[I](env: Term.Env[I], term: Term[I])(implicit si: SourceInfo[I]): Either[String, Term[I]] =
    term match {
      case TmVar(info, name) =>
        env.get(name) match {
          case Some((_term, _)) => Right(_term)
          case _ => Left(si.showMessage(info, s"variable `$name` is not declared"))
        }
      case TmConst(info, name) =>
        this.axioms.get(name) match {
          case Some(sort) => Right(TmConst(si.noInfo, sort))
          case None => Left(si.showMessage(info, s"no axiom for constant `$name`"))
        }
      case TmApp(info, func, arg) => for {
        funcType <- this.typeOf(env, func);
        _funcType <- Term.weakNormalize(env, funcType);
        _ <- eitherMonad.whenM(!_funcType.isProduct) {
          Left(si.showMessage(
            func.info,
            s"function `${func.toString}` has type `${funcType.toString}`" +
            " which is not a product"
          ))
        }
        TmProd(_, paramName, paramType, body) = _funcType;
        argType <- this.typeOf(env, arg);
        _argType <- Term.normalize(env, argType);
        _paramType <- Term.normalize(env, paramType);
        _ <- eitherMonad.whenM(!_argType.alphaEquals(_paramType)) {
          Left(si.showMessage(
            arg.info,
            s"argument `${arg.toString}` has type `${argType.toString}`" +
            s" which does not match parameter type `${paramType.toString}`"
          ))
        }
      } yield body.substitute(paramName, arg)
      case TmAbs(info, paramName, paramType, body) => for {
        termType <-
          if (env.contains(paramName)) {
            val _paramName = Util.getFreshVarName("_", env.keySet)
            val _body = body.renameFreeVar(paramName, _paramName)
            val _env = env + (_paramName -> ((paramType, None)))
            this.typeOf(_env, _body).map {
              TmProd(si.noInfo, _paramName, paramType, _)
            }
          }
          else {
            val _env = env + (paramName -> ((paramType, None)))
            this.typeOf(_env, body).map {
              TmProd(si.noInfo, paramName, paramType, _)
            }
          };
        termTypeType <- this.typeOf(env, termType);
        _termTypeType <- Term.weakNormalize(env, termTypeType);
        _ <- eitherMonad.whenM(!_termTypeType.isConstant) {
          Left(si.showMessage(
            info,
            s"abstraction `${term.toString}` has type `${termType.toString}`" +
            s" which has type `${termTypeType.toString}` which is not a sort"
          ))
        }
        TmConst(_, sort) = _termTypeType;
        _ <- eitherMonad.whenM(!this.sorts.contains(sort)) {
          Left(si.showMessage(
            info,
            s"abstraction `${term.toString}` has type `${termType.toString}`" +
            s" which has type `${termTypeType.toString}` which is not a sort"
          ))
        }
      } yield termType
      case TmProd(info, paramName, paramType, body) => for {
        paramTypeType <- this.typeOf(env, paramType);
        _paramTypeType <- Term.weakNormalize(env, paramTypeType);
        _ <- eitherMonad.whenM(!_paramTypeType.isConstant) {
          Left(si.showMessage(
            paramType.info,
            s"parameter type `${paramType.toString}` has type `${paramTypeType.toString}`" +
            " which is not a sort"
          ))
        }
        TmConst(_, sort1) = _paramTypeType;
        _ <- eitherMonad.whenM(!this.sorts.contains(sort1)) {
          Left(si.showMessage(
            paramType.info,
            s"parameter type `${paramType.toString}` has type `${paramTypeType.toString}`" +
            " which is not a sort"
          ))
        }
        (_env, _body) =
          if (env.contains(paramName)) {
            val _paramName = Util.getFreshVarName("_", env.keySet)
            (env + (_paramName -> ((paramType, None))), body.renameFreeVar(paramName, _paramName))
          }
          else
            (env + (paramName -> ((paramType, None))), body);
        bodyType <- this.typeOf(_env, _body);
        _bodyType <- Term.weakNormalize(_env, bodyType);
        _ <- eitherMonad.whenM(!_bodyType.isConstant) {
          Left(si.showMessage(
            body.info,
            s"body ${body.toString} has `${bodyType.toString}`" +
            " which is not a sort"
          ))
        }
        TmConst(_, sort2) = _bodyType;
        _ <- eitherMonad.whenM(!this.sorts.contains(sort2)) {
          Left(si.showMessage(
            body.info,
            s"body ${body.toString} has `${bodyType.toString}`" +
            " which is not a sort"
          ))
        }
        rule = this.rules.get((sort1, sort2));
        _ <- eitherMonad.whenM(rule.isEmpty) {
          Left(si.showMessage(
            info,
            s"no rule for sorts `$sort1`, `$sort2`"
          ))
        }
      } yield TmConst(si.noInfo, rule.get)
    }
}

object PTS {
  type Sorts = Set[String]
  type Axioms = Map[String, String]
  type Rules = Map[(String, String), String]
}
