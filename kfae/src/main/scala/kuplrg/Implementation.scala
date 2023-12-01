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

  def reduce(k: Cont, s: Stack): (Cont, Stack) = (k,s) match{
    case (EmptyK, s)                                     => (EmptyK, s)
    case (EvalK(env, expr, k),s) => expr match{
      case Num(n)=>(k,NumV(n)::s)
      case Add(l,r) => (EvalK(env, l, EvalK(env, r, AddK(k))),s)
      case Mul(l,r) => (EvalK(env, l, EvalK(env, r, MulK(k))),s)
      case Id(x)=> (k,env.getOrElse(x,error(s"free identifier"))::s)
      case Fun(param, body)=>(k,CloV(param,body,env)::s)
      case App(f,e) => (EvalK(env,f,EvalK(env, e, AppK(k))),s)
      case Vcc(x,b) => (EvalK(env + (x -> ContV(k,s)),b,k) ,s)
    }
    case (AddK(k),r::l::s)=>(k,numAdd(l,r)::s)
    case (MulK(k),r::l::s)=>(k,numMul(l,r)::s)
    case (AppK(k),e::f::s)=> f match{
      case CloV(p,b,fenv) =>{
        (EvalK(fenv + (p->e),b,k),s)
      }
      case ContV(k1,s1) => (k1, e :: s1)
      case _ => error(s"not a function")
    }
  }
}
