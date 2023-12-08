package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Cont.*

  type BOp = (BigInt, BigInt) => BigInt
  def numBOp(x: String)(op: BOp)(l: Value, r: Value): Value = (l, r) match
    case (NumV(l), NumV(r))                         => NumV(op(l, r))
    case (l, r)                                     => error(s"invalid operation ${l.str} $x ${r.str}")

  val numAdd: (Value, Value) => Value = numBOp("+")(_ + _)
  val numMul: (Value, Value) => Value = numBOp("*")(_ * _)

  def interpCPS(expr: Expr, env: Env, k: Value => Value): Value = expr match{
    case Num(n)=>k(NumV(n))
    case Add(l,r)=>
      interpCPS(l,env,{
        lv => interpCPS(r,env,rv => {
          val NumV(n) =lv
          val NumV(m) = rv
          k(NumV(n+m))
        })
      })
    case Mul(l,r)=>
      interpCPS(l,env,{
        lv => interpCPS(r,env,rv => {
          val NumV(n) =lv
          val NumV(m) = rv
          k(NumV(n*m))
        })
      })
    case Id(x) => k(env.getOrElse(x,error("free identifier")))
    case App(f,e) =>interpCPS(f,env,v => v match {
      case CloV(p,b,fenv) => {
        interpCPS(e,env,v=>{
          interpCPS(b,fenv + (p->v),k)
        })
      }
      case v => error("not a function")
      }
    )
    case Fun(p,b)=>k(CloV(p,b,env))
  }

  def reduce(k: Cont, s: Stack): (Cont, Stack) = (k,s) match{
    case (EmptyK, s)                                     => (EmptyK, s)
    case (EvalK(env, expr, k),s) => expr match{
      case Num(n)=>(k,NumV(n)::s)
      case Add(l,r) => (EvalK(env, l, EvalK(env, r, AddK(k))),s)
      case Mul(l,r) => (EvalK(env, l, EvalK(env, r, MulK(k))),s)
      case Id(x)=> (k,env.getOrElse(x,error(s"free identifier"))::s)
      case Fun(param, body)=>(k,CloV(param,body,env)::s)
      case App(f,e) => (EvalK(env,f,EvalK(env, e, AppK(k))),s)
    }
    case (AddK(k),r::l::s)=>(k,numAdd(l,r)::s)
    case (MulK(k),r::l::s)=>(k,numMul(l,r)::s)
    case (AppK(k),e::f::s)=> f match{
      case CloV(p,b,fenv) =>{
        (EvalK(fenv + (p->e),b,k),s)
      }
      case _ => error(s"not a function")
    }
  }

  
  

}
