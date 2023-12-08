package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*

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
    case Fun(p,b) => CloV(p,b, () => env)
    case Rec(n,p,b,s) => {
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
}
