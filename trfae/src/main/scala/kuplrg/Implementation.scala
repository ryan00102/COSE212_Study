package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Type.*

  def typeCheck(expr: Expr, tenv: TypeEnv): Type = expr match{
    case Num(_) => NumT
    case Bool(_) => BoolT
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

    case Div(l,r)=>{
      mustSame(typeCheck(l,tenv),NumT)
      mustSame(typeCheck(r,tenv),NumT)
      NumT
    }

    case Mod(l,r)=>{
      mustSame(typeCheck(l,tenv),NumT)
      mustSame(typeCheck(r,tenv),NumT)
      NumT
    }
    case Eq(l,r)=>{
      mustSame(typeCheck(l,tenv),NumT)
      mustSame(typeCheck(r,tenv),NumT)
      BoolT
    }
    case Lt(l,r)=>{
      mustSame(typeCheck(l,tenv),NumT)
      mustSame(typeCheck(r,tenv),NumT)
      BoolT
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
    case Rec(x,p,pty,rty,body,scope) =>{

      val t2 = typeCheck(body, tenv + (x->ArrowT(pty,rty)) + (p->pty))
      typeCheck(scope, tenv+(x->ArrowT(pty,rty)))
    }

    case If(cond, thenExpr, elseExpr) =>{
      typeCheck(cond, tenv) match 
        case BoolT =>{
          val resultT = typeCheck(elseExpr,tenv)
          mustSame(typeCheck(elseExpr,tenv), typeCheck(thenExpr,tenv))
          resultT
        }
        case _ => error()
    }
  }

  def interp(expr: Expr, env: Env): Value = expr match{
    case Num(n) => NumV(n)
    case Bool(b) => BoolV(b)
    case Id(x) => env.getOrElse(x,error("free identifier"))
    case Add(l,r) => (interp(l,env),interp(r,env)) match{
      case (NumV(l),NumV(r)) => NumV(l+r)
      case (l,r) => error("invalid operation")
    }
    case Mul(l,r) => (interp(l,env),interp(r,env)) match{
      case (NumV(l),NumV(r)) => NumV(l*r)
      case (l,r) => error("invalid operation")
    }
    case Div(l,r) => (interp(l,env),interp(r,env)) match{
      case (NumV(l),NumV(r)) => {
        if (NumV(r) != NumV(0)){
          NumV(l/r)
        }
        else{
          error("invalid operation")
        }
      }
      case (l,r) => error("invalid operation")
    }
    case Mod(l,r) => (interp(l,env),interp(r,env)) match{
      case (NumV(l),NumV(r)) => {
        if (NumV(r) != NumV(0)){
          NumV(l%r)
        }
        else{
          error("invalid operation")
        }
      }
      case (l,r) => error("invalid operation")
    }
    case Eq(l,r) => (interp(l,env),interp(r,env)) match{
      case (NumV(l), NumV(r)) => BoolV(l==r)
      case (l,r) => error("invalid operation")
    }
    case Lt(l,r) => (interp(l,env),interp(r,env)) match{
      case (NumV(l), NumV(r)) => BoolV(l<r)
      case(l,r) => error("invalid operation")
    }
    case Val(x,i,b) => interp(b, env + (x -> interp(i,env)))
    case Id(x) => env.getOrElse(x,error("free identifier"))
    case Fun(p,t,b) => CloV(p,b,() => env)
    
    case Rec(n,p,pty, rty,b,s) => {
      lazy val newEnv : Env = env + (n -> CloV(p,b, () => newEnv))
      interp(s, newEnv)
    }
    case App(f,e) => interp(f,env) match{
      case CloV(p,b,fenv) => interp(b, fenv() + (p -> interp(e,env)))
      case v => error("not a function")
    }
    case If(c,t,e) => interp(c,env) match{
      case BoolV(true) => interp(t,env)
      case BoolV(false) => interp(e,env)
      case v => error("not a boolean")
    }
  }

  def mustSame(lty : Type, rty : Type) : Unit ={
    if(lty!= rty){
      error()
    }
  }

}
