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
      typeCheck(body, tenv.addVar(x -> t1))
    }

    case Id(x) =>{
      tenv.vars.getOrElse(x, error())
    }
    case Fun(p,t,b) =>{
      mustValid(t,tenv)
      val t1 = typeCheck(b, tenv.addVar(p->t))
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

    case TypeAbs(name,body)=>{
      if(tenv.tys.contains(name)) error()
      PolyT(name, typeCheck(body, tenv.addType(name)))
    }

    case TypeApp(expr,ty)=>typeCheck(expr,tenv) match{
      case PolyT(name, bodyTy) => subst(bodyTy, name, mustValid(ty,tenv))
      case t => error()
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
    case TypeAbs(name,body) => TypeAbsV(name, body, env)
    case TypeApp(expr,ty)=> interp(expr,env)match{
      case TypeAbsV(name, body, env) => interp(body,env)
      case v=> error()
    }
  }

  def mustValid(ty : Type,tenv : TypeEnv):Type = {ty match
    case NumT => NumT
    case ArrowT(pty, rty) =>
      ArrowT(mustValid(pty,tenv),mustValid(rty, tenv))
    case VarT(tn)=>
      if(!tenv.tys.contains(tn)) error()
      VarT(tn)
    case PolyT(name , ty)=> PolyT(name, mustValid(ty,tenv.addType(name)))
  }

  def subst(bodyTy:Type, name:String,ty:Type):Type =bodyTy match{
    case NumT => NumT
    case ArrowT(pty, rty) => ArrowT(subst(pty,name, ty),subst(rty,name,ty))
    case VarT(x)=>{
      if(name == x) ty 
      else VarT(x)
    }
    case PolyT(x,bodyTy)=>{
      if(name == x)PolyT(x,bodyTy)
      else PolyT(x,subst(bodyTy,name, ty))
    }
  }
  def mustSame(lty : Type, rty : Type) : Unit ={
    if(!isSame(lty,rty)){
      error()
    }
  }
  def isSame(lty : Type, rty : Type) : Boolean = (lty, rty) match{
    case (NumT, NumT) => true
    case (ArrowT(lpty, lrty),ArrowT(rpty, rrty))=>{
      isSame(lpty, rpty) && isSame(lrty, rrty)
    }
    case (VarT(lname),VarT(rname))=>lname == rname
    case(PolyT(lname, lty),PolyT(rname, rty))=>{
      isSame(lty, subst(rty, rname, VarT(lname)))
    }
    case _ => false
  }

}
