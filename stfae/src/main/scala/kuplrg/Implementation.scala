package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Type.*

  def typeCheck(expr: Expr, tenv: TypeEnv): Type = expr match{
    case Num(n) => NumT
    case Add(l,r) => {
      subtype(typeCheck(l,tenv),NumT)
      subtype(typeCheck(r,tenv),NumT)
      NumT
    }
    case Mul(l,r) => {
      subtype(typeCheck(l,tenv),NumT)
      subtype(typeCheck(r,tenv),NumT)
      NumT
    }
    case Id(x) =>{
      tenv.getOrElse(x,error())
    }
    case Val(x,t0,e1,e2) =>{
      t0 match{
        case Some(t) => {
          subtype(typeCheck(e1,tenv),t)
          typeCheck(e2, tenv + (x->t))
        }
        case None => typeCheck(e2,tenv + (x->typeCheck(e1,tenv)))
      }
    }
    case Fun(x,t,e) =>{
      ArrowT(t, typeCheck(e,tenv + (x->t)))
    }
    case App(e0,e1) => typeCheck(e0,tenv) match {
      case ArrowT(t1,t2) => {
        subtype(typeCheck(e1,tenv),t1)
        t2
      }
      case ty => error()
    }
    case Record(fs) =>{
      RecordT(fs.map{case(f,e) => (f,typeCheck(e,tenv))}.toMap)
    }
    case Access(e,xi) => typeCheck(e,tenv) match{
      case RecordT(fs) => fs.getOrElse(xi,error())
      case ty => error()
    }
    case Exit => BotT
  }

  def interp(expr: Expr, env: Env): Value = expr match {
    case Num(n) => NumV(n)
    case Add(l,r) => (interp(l,env),interp(r,env)) match {
      case (NumV(l),NumV(r)) => NumV(l+r)
      case (l,r) => error()
    }
    case Mul(l,r) =>(interp(l,env),interp(r,env)) match {
      case (NumV(l),NumV(r)) => NumV(l*r)
      case (l,r) => error()
    }
    case Val(x,t0,e1,e2) =>{
      t0 match{
        case Some(t) => interp(e2, env + (x->interp(e1,env)))
        case None => interp(e2, env + (x->interp(e1,env)))
      }
    }
    case Fun(x,t,e) => CloV(x,e,env)
    case Id(x) => env.getOrElse(x,error())
    case App(e0,e1)=> interp(e0,env) match {
      case CloV(x,e2, newEnv) => interp(e2, newEnv + (x->interp(e1,env)))
      case _ => error()
    }
    case Record(fs) => RecordV(fs.map{case (f,e) => (f,interp(e,env))}.toMap)
    case Access(e,xi)=>interp(e,env)match{
      case RecordV(fs) => fs.getOrElse(xi,error())
      case v => error()
    }
    case Exit => error()
  }

  def subtype(t1 : Type, t2 : Type) : Boolean = (t1, t2) match{
    case (_, TopT) => true
    case (BotT, _) => true
    case (NumT, NumT) => true
    case (ArrowT(p1, r1), ArrowT(p2, r2)) =>
      subtype(p2, p1) && subtype(r1, r2)
    case (RecordT(fs1), RecordT(fs2)) =>
      fs2.forall{
        case (x, t2) => fs1.get(x) match {
          case None => false
          case Some(t1) => subtype(t1, t2)
        }
      }
    case (_,_) => false
  }

  def mustValid(ty : Type,tenv : TypeEnv):Type = ty match{
    case NumT => NumT
    case ArrowT(pt,rt) =>{
      ArrowT(mustValid(pt,tenv),mustValid(rt,tenv))
    }
  }
}
