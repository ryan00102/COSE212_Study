package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*

  def interp(expr: Expr, env: Env): Value = expr match {
    case Expr.EUnit => Value.UnitV
    case Expr.ENum(number) => Value.NumV(number)
    case Expr.EBool(bool) => Value.BoolV(bool)
    case Expr.EId(name) =>
      if (env.contains(name)) {
        env(name)
      } else {
        error(s"free identifier: $name")
      }
    case Expr.EAdd(left, right) =>
      (interp(left, env), interp(right, env)) match {
        case (Value.NumV(l), Value.NumV(r)) => Value.NumV(l + r)
        case _ => error("invalid operation")
      }
    case Expr.EMul(left, right) =>
      (interp(left, env), interp(right, env)) match {
        case (Value.NumV(l), Value.NumV(r)) => Value.NumV(l * r)
        case _ => error("invalid operation")
      }
    case Expr.EDiv(left, right) =>
      interp(left, env) match {
        case leftValue: Value.NumV =>
          interp(right, env) match {
            case rightValue: Value.NumV =>
              if (rightValue.number == 0)
                error("invalid operation")
              Value.NumV(leftValue.number / rightValue.number)
            case _ => error("invalid operation")
          }
        case _ => error("invalid operation")
      }

    case Expr.EMod(left, right) =>
      interp(left, env) match {
        case leftValue: Value.NumV =>
          interp(right, env) match {
            case rightValue: Value.NumV =>
              if (rightValue.number == 0)
                error("invalid operation")
              Value.NumV(leftValue.number % rightValue.number)
            case _ => error("invalid operation")
          }
        case _ => error("invalid operation")
      }

    case Expr.EEq(left, right) =>
      (interp(left, env), interp(right, env)) match {

        
        case (Value.CloV(_,_,_), Value.CloV(_,_,_)) =>
          error("invalid operation")
        case (Value.NilV, Value.ConsV(_, _)) => 
          Value.BoolV(false)
        case (Value.ConsV(_, _), Value.NilV) => 
          Value.BoolV(false)

        case (l, r) => 
          if (l.getClass == r.getClass)
            if (l == r) {Value.BoolV(true)}
            else {
              
              Value.BoolV(false)}
          else{
            
            error("invalid operation")
          }
        
      }
    case Expr.ELt(left, right) =>
      (interp(left, env), interp(right, env)) match {
        case (Value.NumV(l), Value.NumV(r)) => Value.BoolV(l < r)
        case _ => error("invalid operation")
      }
    case Expr.EIf(cond, thenExpr, elseExpr) =>
      interp(cond, env) match {
        case Value.BoolV(true) => interp(thenExpr, env)
        case Value.BoolV(false) => interp(elseExpr, env)
        case _ => error("not a boolean")
      }

    case Expr.ENil => Value.NilV

    case Expr.ECons(head, tail) =>
      val interpretedHead = interp(head,env)
      val interpretedTail = interp(tail,env)
      Value.ConsV(interpretedHead, interpretedTail)
    case Expr.EHead(list) =>
      interp(list, env) match {
        case Value.ConsV(h, _) => h
        case Value.NilV => error("empty list")
        case _ => error("not a list")
      }
    case Expr.ETail(list) =>
      interp(list, env) match {
        case Value.ConsV(_, t) => t
        case Value.NilV => error("empty list")
        case _ => error("not a list")
      }
    case Expr.ELength(list) =>
      interp(list, env) match {
        case Value.NilV => Value.NumV(0)
        case Value.ConsV(_, t) => Value.NumV(1 + length(t))
        case _ => error("not a list")
      }
    case Expr.EMap(list, fun) =>
      val listValue = interp(list, env)
      val funValue = interp(fun, env)
      listValue match {
        case Value.NilV => Value.NilV
        case _ => map(listValue, funValue)
      }
    case Expr.EFlatMap(list, fun) =>
        val listValue = interp(list, env)
        val funValue = interp(fun, env)
        listValue match {
          case Value.NilV => Value.NilV
          case _ =>
            join(map(listValue, funValue)) 
        }

    case Expr.EFilter(list, fun) =>
      val listValue = interp(list, env)
      val funValue = interp(fun, env)
      listValue match {
        case Value.NilV => Value.NilV
        case _ =>
          filter(listValue, funValue)
      }

    case Expr.ETuple(exprs) =>
      Value.TupleV(exprs.map(e => interp(e, env)))

    case Expr.EProj(tuple, index) =>
      interp(tuple, env) match {
        case Value.TupleV(values) =>
          if (index >= 1 && index <= values.length) {
            values(index-1)
          } else {
            error("out of bounds")
          }
        case _ => error("not a tuple")
      }

    case Expr.EVal(name, value, scope) =>
      val evaluatedValue = interp(value, env)
      val newEnv = env + (name -> evaluatedValue)
      interp(scope, newEnv)

    case Expr.EFun(params, body) =>
      Value.CloV(params, body, ()=>env)

    case Expr.ERec(defs, scope) =>
      lazy val newEnv : Env = defs.foldLeft(env) {
        case (currentEnv, FunDef(name, params, funcBody)) =>
          val closure = Value.CloV(params, funcBody, () => newEnv)
          currentEnv + (name -> closure)
      }

      interp(scope, newEnv)

      
    case Expr.EApp(fun, args) =>
      val funValue = interp(fun, env)
      val argValues = args.map(arg => interp(arg, env))

      app(funValue, argValues)

    
  }

  def eq(left: Value, right: Value): Boolean = (left, right) match {
     case (l, r) => 
      if (l.getClass != r.getClass)
        error("invalid operation")
      else
        (left, right) match {
          case (Value.UnitV, Value.UnitV) => true
          case (Value.NumV(n1), Value.NumV(n2)) => n1 == n2
          case (Value.BoolV(b1), Value.BoolV(b2)) => b1 == b2
          case (Value.NilV, Value.NilV) => true

          case (Value.ConsV(head1, tail1), Value.ConsV(head2, tail2)) =>
            eq(head1, head2) && eq(tail1, tail2)

          case (Value.NilV, Value.ConsV(_, _)) => false
          case (Value.ConsV(_, _), Value.NilV) => false

          case (Value.TupleV(values1), Value.TupleV(values2)) =>
            if (values1.length != values2.length) {
              false
            } else {
              values1.zip(values2).forall { case (v1, v2) => eq(v1, v2) }
            }

          case _ => false
        }

    
  }

  def length(list: Value): BigInt = list match {
    case Value.NilV => 0
    case Value.ConsV(_, tail) => 1 + length(tail)
    case _ => error("not a list")
  }

  def map(list: Value, fun: Value): Value = list match {
    case Value.NilV => Value.NilV

    case Value.ConsV(head, tail) =>
      val mappedHead = app(fun, List(head))
      val mappedTail = map(tail, fun)
      Value.ConsV(mappedHead, mappedTail)

    case _ =>
      error("not a list")
  }

  def join(list: Value): Value = list match {
    case Value.NilV => Value.NilV

    case Value.ConsV(Value.NilV, tail) =>
      join(tail)

    case Value.ConsV(Value.ConsV(head, tail), rest) =>
      Value.ConsV(head, join(Value.ConsV(tail, rest)))

    case _ =>
      error("not a list")
  }

  def filter(list: Value, fun: Value): Value = list match {
    case Value.NilV => Value.NilV

    case Value.ConsV(head, tail) =>
      val filterHead = app(fun, List(head))
      if (filterHead == Value.BoolV(true)) {
        Value.ConsV(head, filter(tail, fun))
      } else {
        filterHead match{
          case Value.BoolV(b) => filter(tail, fun)
          case _ => error("not a boolean")
        }
        
      }

    case _ =>
      error("not a list")
  }


  def app(fun: Value, args: List[Value]): Value = fun match {
    case Value.CloV(params, body, env) =>
      if (params.length <= args.length) {
        val extendedEnv = params.zip(args).foldLeft(env()) {
          case (currentEnv, (param, arg)) => currentEnv + (param -> arg)
        }
        interp(body, extendedEnv)
      } else {
        val remainingParams = params.drop(args.length)
        val extendedEnv = params.zip(args).foldLeft(env()) {
          case (currentEnv, (param, arg)) => currentEnv + (param -> arg)
        }
        val unusedParams = remainingParams.map(param => (param, Value.UnitV))
        val finalEnv = extendedEnv ++ unusedParams
        interp(body, finalEnv)
      }

    case _ =>
      error("not a function")
  }

  
  

 

  
  
}
  