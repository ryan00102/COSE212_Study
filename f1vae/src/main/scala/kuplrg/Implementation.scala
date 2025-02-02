package kuplrg

object Implementation extends Template {

  import Expr.*

  def interp(expr: Expr, env: Env, fenv: FEnv): Value = expr match{
    case Num(n) => n
    case Add(l,r) => interp(l,env, fenv) + interp(r,env, fenv)
    case Mul(l,r) => interp(l,env, fenv) * interp(r,env, fenv)
    case Val(x,i,b) => interp(b ,env + (x->interp(i,env, fenv)),fenv)
    case Id(x) => env.getOrElse(x,error("free identifier"))
    case App(f,s) =>{
      val fdef = fenv.getOrElse(f,error("unknown function"))
      interp(fdef.body, Map(fdef.param -> interp(s,env,fenv)),fenv)
    }

  }

  def interpDS(expr: Expr, env: Env, fenv: FEnv): Value = expr match{
    case Num(n) => n
    case Add(l,r) => interpDS(l,env, fenv) + interpDS(r,env, fenv)
    case Mul(l,r) => interpDS(l,env, fenv) * interpDS(r,env, fenv)
    case Val(x,i,b) => interpDS(b ,env + (x->interpDS(i,env, fenv)),fenv)
    case Id(x) => env.getOrElse(x,error("free identifier"))
    case App(f,s) =>{
      val fdef = fenv.getOrElse(f,error("unknown function"))
      interpDS(fdef.body, env + (fdef.param -> interpDS(s,env,fenv)),fenv)
    }
  }
}
