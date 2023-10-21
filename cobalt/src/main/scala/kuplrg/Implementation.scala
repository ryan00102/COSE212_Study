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
        case _ => error("invalid operation")
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
      (interp(list, env),interp(fun, env)) match {
        case(Value.NilV,_) => 
          Value.NilV
        case (Value.ConsV(head, tail),Value.CloV(p, b, e)) =>
          val mappedHead = interp(Expr.EMap(fun, valueToExpr(head)), env)
          val mappedTail = interp(Expr.EMap(fun,valueToExpr(tail)),env)
          Value.ConsV(mappedHead, mappedTail)
        case (Value.ConsV(head, tail),_) => error("not a function")
        case (_,Value.CloV(p, b, e)) => error("not a list")
        case _ => error("not a list")
      }
    case Expr.EFlatMap(list, fun) =>
      (interp(list, env),interp(fun, env)) match {
        case (Value.NilV,_) => Value.NilV
        case (Value.ConsV(head, tail),Value.CloV(p, b, e)) =>
          val mappedHead = interp(Expr.EApp(fun, List(valueToExpr(head))), env)
          val flatMappedTail = interp(Expr.EFlatMap(valueToExpr(tail), fun), env)
          interp(Expr.ECons(valueToExpr(mappedHead), valueToExpr(flatMappedTail)), env)
        case (Value.ConsV(head, tail),_) => error("not a function")
        case (_,Value.CloV(p, b, e)) => error("not a list")
        case _ => error("not a list")
      }
    case Expr.EFilter(list, fun) =>
      (interp(list, env),interp(fun, env)) match {
        case (Value.NilV,_) => Value.NilV
        case (Value.ConsV(head, tail),Value.CloV(p, b, e)) =>
          val filterHead = interp(Expr.EApp(fun, List(valueToExpr(head))), env)
          if (filterHead == Value.BoolV(true)) {
            Value.ConsV(head, interp(Expr.EFilter(valueToExpr(tail), fun), env))
          } else {
            interp(Expr.EFilter(valueToExpr(tail), fun), env)
          }
        case (Value.ConsV(head, tail),_) => error("not a function")
        case (_,Value.CloV(p, b, e)) => error("not a list")
        case _ => error("not a list")
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
    // case ERec(defs, scope) =>
    //   val newEnv = env ++ defs.map(d => (d.name, Value.CloV(d.params, d.body, () => newEnv)))
    //   interp(scope, newEnv)
      

      
    case Expr.EApp(fun, args) =>
      val funValue = interp(fun, env)
      val argValues = args.map(arg => interp(arg, env))
      funValue match {
        case Value.CloV(params, body, cloEnv) =>
          if (params.length == argValues.length) {
            val newEnv = cloEnv() ++ params.zip(argValues)
            interp(body, newEnv)
          } else {
            error("invalid operation")
          }
        case _ =>
          error("not a function")
      }
    
  }
  def valueToExpr(value: Value): Expr = value match {
    case Value.NumV(num) => Expr.ENum(num)
    case Value.BoolV(bool) => Expr.EBool(bool)
  
  }

 

  def length(list: Value): BigInt = list match {
    case Value.NilV => 0
    case Value.ConsV(_, tail) => 1 + length(tail)
    case _ => error("not a list")
  }
  
}
  