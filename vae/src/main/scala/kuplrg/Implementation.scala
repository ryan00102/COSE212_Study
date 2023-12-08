package kuplrg

object Implementation extends Template {

  import Expr.*

  def interp(expr: Expr, env: Env): Value = expr match {
    case Num(n) => n
    case Add(l,r) => interp(l,env) + interp(r,env)
    case Mul(l,r) => interp(l,env) * interp(r,env)
    case Val(x,i,b) => {
      interp(b,env + (x -> interp(i,env)))
    }
    case Id(x) => {
      env.getOrElse(x, error("free identifier"))
    }
  }

  def freeIds(expr: Expr): Set[String] = 
    expr match {
  
    // expr match{
    //   case Id(x) => {
    //     If()
    //   }
    // }
      case Num(n) => Set()
      case Add(l,r) => freeIds(l) ++ freeIds(r)
      case Mul(l,r) => freeIds(l) ++ freeIds(r)
      case Val(x,i,b) => {
        freeIds(b)
      }
      case Id(x) => {
        if(bindingIds(expr).contains(x)) Set()
        else Set(x)
      }
  }

  def bindingIds(expr: Expr): Set[String] = {
    expr match{
      case Num(n) => Set()
      case Add(l,r) => bindingIds(l)++bindingIds(r)
      case Mul(l,r) => bindingIds(l)++bindingIds(r)
      case Val(x,i,b) => {
        Set(x) ++bindingIds(i)++ bindingIds(b)
      }
      case Id(x) => Set()
    }
  }

  def boundIds(expr: Expr): Set[String] = expr match {
    case Val(x, i, b) =>
      Set(x) ++ boundIds(i) ++ boundIds(b)
    case _ => Set() 
  }

  def shadowedIds(expr: Expr): Set[String] = ???

}
