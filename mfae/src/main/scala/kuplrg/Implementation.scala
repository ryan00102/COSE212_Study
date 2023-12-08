package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  
  def lookupId(env : Env, name : String) : Addr = {
    env.getOrElse(name, error("free identifier"))
  }
  
  def malloc(mem : Mem) : Addr = mem.keySet.maxOption.fold(0)(_+1)

  def interp(expr: Expr, env: Env, mem: Mem): (Value, Mem) = expr match{
    case Num(n) => (NumV(n),mem)
    case Add(l,r) => (interp(l,env,mem),interp(r,env,mem)) match{
      case((NumV(l),mem1),(NumV(r),mem2)) => (NumV(l+r),mem2)
      case (l,r) => error("invalid operation")
    }
    case Mul(l,r) => (interp(l,env,mem),interp(r,env,mem)) match{
      case((NumV(l),mem1),(NumV(r),mem2)) => (NumV(l*r),mem2)
      case (l,r) => error("invalid operation")
    }
    case Id(x) => (mem(env.getOrElse(x,error("free identifier"))),mem)
    case Fun(p,b) => (CloV(p,b,env),mem)
    case App(f,a) =>{
      val (fv, fmem) = interp(f,env,mem)
      fv match{
        case CloV(p,b,fenv) => {
          val (av,amem) = interp(a,env,fmem)
          val addr = malloc(amem)
          interp(b,fenv + (p->addr), amem + (addr -> av))
        }
        case _ => error("not a function")
      }
    }
    case Var(x,i,b) => {
      val (iv, imem) = interp(i,env,mem)
      val addr = malloc(imem)
      interp(b, env + (x -> addr ),imem + (addr -> iv))
    }
    case Assign(x,e) =>{
      val (ev,emem) = interp(e,env,mem)
      (ev, emem + (lookupId(env, x) -> ev))
    }
    case Seq(l,r) =>{
      val (_, mem1) = interp(l,env,mem)
      val (v2, mem2) = interp(r,env,mem1)
      interp(r,env, mem2)
    }
  }
  

  def interpCBR(expr: Expr, env: Env, mem: Mem): (Value, Mem) = expr match{
    case Num(n) => (NumV(n),mem)
    case Add(l,r) => (interpCBR(l,env,mem),interpCBR(r,env,mem)) match{
      case((NumV(l),mem1),(NumV(r),mem2)) => (NumV(l+r),mem2)
      case (l,r) => error("invalid operation")
    }
    case Mul(l,r) => (interpCBR(l,env,mem),interpCBR(r,env,mem)) match{
      case((NumV(l),mem1),(NumV(r),mem2)) => (NumV(l*r),mem2)
      case (l,r) => error("invalid operation")
    }
    case Id(x) => (mem(env.getOrElse(x,error("free identifier"))),mem)
    case Fun(p,b) => (CloV(p,b,env),mem)
    case App(f,a) =>{
      val (fv, fmem) = interpCBR(f,env,mem)
      fv match{
        case CloV(p,b,fenv) => a match{
          case Id(name) =>{
            val addr = lookupId(env,name)
            interpCBR(b,fenv + (p->addr), fmem)
          }
          case _ =>{
            val (av,amem) = interpCBR(a,env,fmem)
            val addr = malloc(amem)
            interpCBR(b,fenv + (p->addr), amem + (addr -> av))
          }
        }
        case _ => error("not a function")
      }
    }
    case Var(x,i,b) => {
      val (iv, imem) = interpCBR(i,env,mem)
      val addr = malloc(imem)
      interpCBR(b, env + (x -> addr ),imem + (addr -> iv))
    }
    case Assign(x,e) =>{
      val (ev,emem) = interpCBR(e,env,mem)
      (ev, emem + (lookupId(env, x) -> ev))
    }
    case Seq(l,r) =>{
      val (_, mem1) = interpCBR(l,env,mem)
      val (v2, mem2) = interpCBR(r,env,mem1)
      interpCBR(r,env, mem2)
    }
  }
  
}
