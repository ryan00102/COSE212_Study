package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Inst.*
  import Control.*

  // ---------------------------------------------------------------------------
  // Problem #1
  // ---------------------------------------------------------------------------
  def reduce(st: State): State = st match

      case State(Nil, v::Nil, h,mem) =>State(Nil,v::Nil,h,mem)

      case State(IEval(env,expr)::restK,s,h,mem) => expr match
        
        case EUndef =>
          State(restK, UndefV :: s, h, mem)
        case ENum(n) =>
          State(restK, NumV(n) :: s,h,mem)
        case EBool(b)=>
          State(restK, BoolV(b) :: s,h,mem)
        case EAdd(e1, e2)=>
          State(IEval(env, e1) :: IEval(env, e2) :: IAdd :: restK, s, h, mem)
        case EMul(e1, e2) =>
          State(IEval(env, e1) :: IEval(env, e2) :: IMul :: restK, s, h, mem)
        case EDiv(e1, e2) =>
          State(IEval(env, e1) :: IEval(env, e2) :: IDiv :: restK, s, h, mem)
        case EMod(e1, e2) =>
          State(IEval(env, e1) :: IEval(env, e2) :: IMod :: restK, s, h, mem)
        case EEq(e1, e2) =>
          State(IEval(env, e1) :: IEval(env, e2) :: IEq :: restK, s, h, mem)
        case ELt(e1, e2) =>
          State(IEval(env, e1) :: IEval(env, e2) :: ILt :: restK, s, h, mem)
        case EVar(x, e1, e2)=>
          State(IEval(env, e1) :: IDef(x::Nil,env,e2)::restK,s,h,mem)
        case EId(x) =>
          State(restK, mem(lookup(env,x))::s,h,mem)
        case EAssign(x,e) =>
          State(IEval(env,e)::IWrite(lookup(env,x)) :: restK,s,h,mem)
        case ESeq(l,r) =>
          State(IEval(env, l)::IPop::IEval(env,r) :: restK,s,h,mem)
        case EIf(c,t,e) =>
          State(IEval(env,c) :: IJmpIf(KValue(IEval(env,t)::restK, s,h)) ::IEval(env,e)::restK,s,h,mem)
        case EWhile(c,b) =>
          val break = KValue(restK, s,h)
          val continue = KValue(IPop :: IEval(env,EWhile(c,b))::restK, s,h)
          val hbody = h + (Continue ->continue) + (Break -> break)
          val body = KValue(IEval(env,b) :: IJmp(Continue) ::Nil, s, hbody)
          State(IEval(env,c)::IJmpIf(body)::restK, UndefV::s,h,mem)
        case EBreak =>
          State(IJmp(Break) ::Nil,UndefV::s,h,mem)
        case EContinue =>
          State(IJmp(Continue) ::Nil,UndefV::s,h,mem)
        case EFun(ps,b) =>
          State(restK, CloV(ps,b,env)::s,h,mem)
        case EApp(f,es) =>
          f match
            case ENum(n) => 
              error()
            case _ =>
              val newK = IEval(env,f) :: es.map(e => IEval(env,e)) ::: ICall(es.length) :: restK
              State(newK, s,h,mem)
          
        case EReturn(e)=>
          State(IEval(env,e) :: IReturn::restK,s,h,mem)
        case ETry(b,x,c) =>
          val fin = KValue(restK, s,h)
          val thr= KValue(IDef(List(x), env, c) :: restK,s,h)
          val hbody = h + (Throw -> thr) + (Finally -> fin)
          State(IEval(env,b) :: IJmp(Finally) :: Nil, s,hbody,mem)
        case EThrow(e) =>
          State(IEval(env,e) :: IJmp(Throw)::Nil, s,h,mem)
        case EGen(ps,b) =>
          State(restK, GenV(ps,b,env) :: s ,h,mem)
        case EIterNext(i,a)=>
          a match
            case None => 
              State(IEval(env,i)::IEval(env,EUndef) :: INext :: restK,s,h,mem)
            case Some(v) =>
              State(IEval(env,i) :: IEval(env,v)::INext::restK,s,h,mem)
       
        case EYield(e) =>
          val hnext = ContV(KValue(restK,s,h))
          State(IEval(env,e) :: IYield :: Nil,BoolV(false)::hnext::s,h,mem)
        case EValueField(r)=>
          State(IEval(env,r)::IValueField::restK,s,h,mem)
        case EDoneField(r)=>
          State(IEval(env,r)::IDoneField::restK ,s,h,mem)

      case State(IAdd :: restK, NumV(n2) :: NumV(n1) :: restS, h, mem) =>
        val result = NumV(n1 + n2)
        State(restK, result :: restS, h, mem)
      case State(IMul :: restK, NumV(n2) :: NumV(n1) :: restS, h, mem) =>
        val result = NumV(n1 * n2)
        State(restK, result :: restS, h, mem)
      case State(IDiv :: restK, NumV(n2) :: NumV(n1) :: restS, h, mem) => 
        n2 match
          case 0 =>
            error()
          case _ => 
            val result = NumV(n1 / n2)
            State(restK, result :: restS, h, mem)

      case State(IMod :: restK, NumV(n2) :: NumV(n1) :: restS, h, mem) => 
        n2 match
          case 0 =>
            error()
          case _ => 
            val result = NumV(n1 % n2)
            State(restK, result :: restS, h, mem)

      case State(IEq :: restK, lv :: rv :: restS, h, mem) =>
        val result = BoolV(eq(lv, rv))
        State(restK, result :: restS, h, mem)

      case State(ILt :: restK, p1::p2 ::restS, h, mem)=>
        (p1, p2) match
          case (NumV(n2), NumV(n1)) => 
            State(restK, BoolV(n2 > n1)::restS,h,mem)
          case _ =>
            error()

      case State(IDef(vars, env, body) :: restK, s, h, mem) => 
        val n : Int = vars.length
        val addrList = malloc(mem, n)
        val (varList, stack) = s.splitAt(n)
        val newEnv = vars.zip(addrList).map((k, v) => (k -> v)).foldLeft(env)(_ + _)
        val newMem = addrList.zip(varList.reverse).map((k, v) => (k -> v)).foldLeft(mem)(_ + _)
        State(IEval(newEnv,body) :: restK,stack,h,newMem)
     

      case State(IWrite(a) :: restK, v :: s, h, mem)=> 
        val newMem = mem + (a -> v)
        State(restK, v::s, h, newMem)

      case State(IPop :: restK, v :: s, h, mem) =>
        State(restK,s,h,mem)

      case State(IJmpIf(kv) :: restK,v::s,h,mem)=> 
        v match 
          case BoolV(true) =>
            State(kv.cont, kv.stack,kv.handler,mem)
          case BoolV(false)=>
            State(restK, s,h,mem)
          case _ =>
            error()

      case State(IJmp(c) :: restK,s,h,mem) =>
        s match
          case v::restS =>
            val KValue(newK, newS, newH) = lookup(h,c)
            if (h.contains(Yield))
              val newnewH = newH + (Yield->lookup(h,Yield))
              State(newK, v:: newS, newnewH, mem)
            else
              val newnewH = newH
              State(newK, v::newS,newnewH, mem)
          case Nil => 
            error()

      case State(ICall(n) :: k, s, h, mem) =>
        val (varList, stack) = s.splitAt(n)
        stack match
          case CloV(params, body, env) :: s  => 
            val m = params.length
            val sbody = {
              if (n >= m)
                varList.drop(n - m) ++ Nil
              else
                List.fill(m - n)(UndefV) ++ varList ++ Nil
            }
            val hbody = h + (Return -> KValue(k, s, h)) - Break - Continue - Yield
            State(IDef(params, env, EReturn(body)) :: Nil, sbody, hbody, mem)
                                                              
          case GenV(params, body, env) :: s =>
            val m = params.length
            val addr = malloc(mem)
            val kbody = IPop :: IDef(params, env, EReturn(ETry(body, "x", EId("x")))) :: Nil
            val sbody = {
              if (n >= m)
                varList.drop(n - m) ++ Nil
              else
                List.fill(m - n)(UndefV) ++ varList ++ Nil
            }
            val phi_body = ContV(KValue(kbody, sbody, Map[Control, KValue]()))
            State(k, IterV(addr) :: s, h, mem + (addr -> phi_body))

          case _                                          => error(s"invalid operation")


      case State(IReturn:: rest,s,h,mem) =>
        s match
          case v::restS =>
            if(h.contains(Yield))
              val hdone = ContV(KValue(IReturn::Nil,Nil, Map[Control, KValue]()))
              State(IYield :: Nil,v:: BoolV(true)::hdone::restS,h,mem)
            else
              State(IJmp(Return)::Nil, v::Nil, h,mem)
          case Nil => 
            error()


      case State(INext :: restK,v::IterV(a)::s,h,mem)=>
        mem(a) match
          case ContV(KValue(newK, newS, newH)) =>
            val phi = KValue(restK, IterV(a) :: s, h)
            val hbody = newH + (Yield -> phi) + (Return -> phi)
            State(newK, v :: newS, hbody, mem)
          case _ =>
            error()


      case State(IYield ::_,s,h,mem) =>
        s match
          case v1::BoolV(b)::v2::restS =>
            h(Yield) match
              case KValue(newK, IterV(a) :: newS,newH)=>
                State(newK, ResultV(v1,b)::newS,newH,mem+(a->v2))
              case _ => error()
          case _ =>
            error()


      case State(IValueField :: restK,s,h,mem) =>
        s match
          case ResultV(v,_) :: restS =>
            State(restK, v::restS,h,mem)
          case _ =>
            error()

      case State(IDoneField :: restK,s,h,mem) =>
        s match
          case ResultV(_,d)::restS =>
            State(restK,BoolV(d)::restS,h,mem)
          
          case _ => error()

      case _ => error()


  // ---------------------------------------------------------------------------
  // Problem #2
  // ---------------------------------------------------------------------------
  def bodyOfSquares: String = s"""
    while (from <= to) {
      yield from *
      from++
    }
  """

  // ---------------------------------------------------------------------------
  // Helper functions
  // ---------------------------------------------------------------------------
  def malloc(mem: Mem, n: Int): List[Addr] =
    val a = malloc(mem)
    (0 until n).toList.map(a + _)

  def malloc(mem: Mem): Addr = mem.keySet.maxOption.fold(0)(_ + 1)

  def lookup(env: Env, x: String): Addr =
    env.getOrElse(x, error(s"free identifier: $x"))

  def lookup(handler: Handler, x: Control): KValue =
    handler.getOrElse(x, error(s"invalid control operation: $x"))

  def eq(l: Value, r: Value): Boolean = (l, r) match
    case (UndefV, UndefV) => true
    case (NumV(l), NumV(r)) => l == r
    case (BoolV(l), BoolV(r)) => l == r
    case (IterV(l), IterV(r)) => l == r
    case (ResultV(lv, ld), ResultV(rv, rd)) => eq(lv, rv) && ld == rd
    case _ => false
}

  