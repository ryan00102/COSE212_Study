package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Type.*

  def typeCheck(expr: Expr, tenv: TypeEnv): Type = expr match{
    case Num(_) => NumT
    case Add(l,r) =>{
      mustSame(typeCheck(l,tenv),NumT)
      mustSame(typeCheck(r,tenv),NumT)
      NumT
    }
    case Mul(l,r)=>{
      mustSame(typeCheck(l,tenv),NumT)
      mustSame(typeCheck(r,tenv),NumT)
      NumT
    }
    case Val(x,init,body) =>{
      val t1 = typeCheck(init,tenv)
      typeCheck(body, tenv + (x -> t1))
    }

    case Id(x) =>{
      tenv.getOrElse(x,error())
    }
    case Fun(p,t,b) =>{
      val t1 = typeCheck(b, tenv + (p->t))
      ArrowT(t,t1)
    }
    case App(f,arg) =>{
      typeCheck(f,tenv) match 
        case ArrowT(pT, rT) =>{
          mustSame(typeCheck(arg,tenv), pT)
          rT
        }
        case ty => error()
    }
  }

  def interp(expr: Expr, env: Env): Value = expr match {
    case Num(n) => NumV(n)
    case Add(l,r) => (interp(l,env),interp(r,env)) match{
      case (NumV(l),NumV(r)) => NumV(l+r)
      case (l,r) => error("invalid operation")
    }
    case Mul(l,r) => (interp(l,env),interp(r,env)) match{
      case (NumV(l),NumV(r)) => NumV(l*r)
      case (l,r) => error("invalid operation")
    }
    case Val(x,i,b) => interp(b, env + (x -> interp(i,env)))
    case Id(x) => env.getOrElse(x,error("free identifier"))
    case Fun(p,t,b) => CloV(p,b,env)
    case App(f,e) => interp(f,env) match {
      case CloV(p,b,fenv) => interp(b, fenv + (p -> interp(e,env)))
      case v => error("not a function")
    }
  }
  def mustSame(lty : Type, rty : Type) : Unit ={
    if(lty!= rty){
      error()
    }
  }
}
