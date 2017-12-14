package pts

case class PTS(sorts: PTS.Sorts, axioms: PTS.Axioms, rules: PTS.Rules) {
  def typeOf[I](env: Term.Env[Option[I]], term: Term[Option[I]]): Term[Option[I]] = term match {
    case TmVar(info, name) =>
      env.get(name) match {
        case Some((_term, _)) => _term
        case _ => throw PTS.typeError(info, s"variable `$name` is not declared")
      }
    case TmConst(info, name) =>
      this.axioms.get(name) match {
        case Some(sort) => TmConst(None, sort)
        case None => throw PTS.typeError(info, s"no axiom for constant `$name`")
      }
    case TmApp(info, func, arg) => {
      val funcType = Term.weakNormalize(env, this.typeOf(env, func))
      funcType match {
        case TmProd(_, paramName, paramType, body) => {
          val _paramType = Term.normalize(env, paramType)
          val argType = Term.normalize(env, this.typeOf(env, arg))
          if (argType.alphaEquals(_paramType)) {
            body.substitute(paramName, arg)
          }
          else {
            throw PTS.typeError(
              arg.info,
              s"argument `${arg.toString}` has type `${argType.toString}`" +
              s" which does not match parameter type `${paramType.toString}`"
            )
          }
        }
        case _ => throw PTS.typeError(
            func.info,
            s"function `${func.toString}` has type `${funcType.toString}`" +
            " which is not a product"
          )
      }
    }
    case TmAbs(info, paramName, paramType, body) => {
      val termType =
        if (env.contains(paramName)) {
          val _paramName = Util.getFreshVarName("_", env.keySet)
          val _body = body.renameFreeVar(paramName, _paramName)
          val _env = env + (_paramName -> ((paramType, None)))
          val bodyType = this.typeOf(_env, _body)
          TmProd(None, _paramName, paramType, bodyType)
        }
        else {
          val _env = env + (paramName -> ((paramType, None)))
          val bodyType = this.typeOf(_env, body)
          TmProd(None, paramName, paramType, bodyType)
        }
      val termTypeType = Term.weakNormalize(env, this.typeOf(env, termType))
      termTypeType match {
        case TmConst(_, sort) =>
          if (this.sorts.contains(sort))
            termType
          else
            throw PTS.typeError(
              info,
              s"abstraction `${term.toString}` has type `${termType.toString}`" +
              s" which has type `${termTypeType.toString}` which is not a sort"
            )
        case _ => throw PTS.typeError(
            info,
            s"abstraction `${term.toString}` has type `${termType.toString}`" +
            s" which has type `${termTypeType.toString}` which is not a sort"
          )
      }
    }
    case TmProd(info, paramName, paramType, body) => {
      val paramTypeType = Term.weakNormalize(env, this.typeOf(env, paramType))
      paramTypeType match {
        case TmConst(_, sort1) =>
          if (this.sorts.contains(sort1)) {
            val bodyType =
              if (env.contains(paramName)) {
                val _paramName = Util.getFreshVarName("_", env.keySet)
                val _body = body.renameFreeVar(paramName, _paramName)
                val _env = env + (_paramName -> ((paramType, None)))
                Term.weakNormalize(_env, this.typeOf(_env, _body))
              }
              else {
                val _env = env + (paramName -> ((paramType, None)))
                Term.weakNormalize(_env, this.typeOf(_env, body))
              }
            bodyType match {
              case TmConst(_, sort2) =>
                if (this.sorts.contains(sort2))
                  this.rules.get((sort1, sort2)) match {
                    case Some(sort3) => TmConst(None, sort3)
                    case None => throw PTS.typeError(info, s"no rule for sorts `$sort1`, `$sort2`")
                  }
                else
                  throw PTS.typeError(
                    body.info,
                    s"body ${body.toString} has `${bodyType.toString}`" +
                    " which is not a sort"
                  )
              case _ =>
                throw PTS.typeError(
                  body.info,
                  s"body ${body.toString} has `${bodyType.toString}`" +
                  " which is not a sort"
                )
            }
          }
          else
            throw PTS.typeError(
              paramType.info,
              s"parameter type `${paramType.toString}` has type `${paramTypeType.toString}`" +
              " which is not a sort"
            )
        case _ => throw PTS.typeError(
            paramType.info,
            s"parameter type `${paramType.toString}` has type `${paramTypeType.toString}`" +
            " which is not a sort"
          )
      }
    }
  }
}

object PTS {
  type Sorts = Set[String]
  type Axioms = Map[String, String]
  type Rules = Map[(String, String), String]

  private def typeError[I](info: Option[I], message: String): RuntimeException = {
    val prefix = info match {
      case Some(i) => i.toString + ": "
      case None => ""
    }
    new RuntimeException(prefix + message)
  }
}
