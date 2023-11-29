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
      typeCheck(body, tenv.addVar(x -> t1))
    }

    case Id(x) =>{
      tenv.vars.getOrElse(x, error())
    }
    case Fun(params,b) =>{
      val ptys = params.map(_.ty)
      for (pty <- ptys){
        mustValid(pty,tenv)
      }
      val rty = typeCheck(b, tenv.addVars(params.map(p=>p.name ->p.ty)))
      ArrowT(ptys,rty)
    }
    case App(f,args) =>{
      typeCheck(f,tenv) match 
        case ArrowT(pTs, rT) =>{
          if(pTs.length != args.length){
            error()
          }
          (pTs zip args).map((p,a)=>mustSame(typeCheck(a,tenv), p))
          
          rT
        }
        case ty => error()
    }
    case Rec(x,params, rty,body, scope) =>{
      val ptys = params.map(_.ty)
      for(pty <- ptys){
        mustValid(pty,tenv)
      }
      mustValid(rty,tenv)
      val fty = ArrowT(ptys,rty)
      typeCheck(body, tenv.addVar(x->fty).addVars(params.map(p=>p.name->p.ty)) )
      typeCheck(scope, tenv.addVar(x ->fty))
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
    case TypeDef(name, varts, body)=>{
      if(tenv.tys.contains(name)) error()
      val newTenv = tenv.addType(name, varts.map(vart => vart.name->vart.ptys).toMap)
      for (vart<-varts; pty<-vart.ptys) mustValid(pty,newTenv)
      mustValid(typeCheck(body, newTenv.addVars(varts.map(vart => vart.name -> ArrowT(vart.ptys,NameT(name))))),tenv)
    }
    case Match(expr, cs) => typeCheck(expr, tenv)match {
      case NameT(tn) => {
        val ts = tenv.tys.getOrElse(tn, error())
        val xs = cs.map(_.name).toSet
        if(ts.keySet != xs || xs.size != cs.length) error()
        cs.map{
          case MatchCase(x,ps,b)=>typeCheck(b,tenv.addVars(ps zip ts(x)))
        }.reduce((lty,rty)=>{
          mustSame(lty,rty); lty
        })
      }
      case _ => error()
    }
  }


  def interp(expr: Expr, env: Env): Value = expr match{
    case Num(n) => NumV(n)
    case Bool(b) => BoolV(b)
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
    case Fun(params,b) => CloV(params.map(_.name),b,() => env)
    
    case Rec(n,params, rty,b,s) => {
      lazy val newEnv : Env = env + (n -> CloV(params.map(_.name),b, () => newEnv))//params는 list of params
      interp(s, newEnv)
    }
    case App(f,es) => interp(f,env) match{
      case CloV(ps,b,fenv) => {
        val vals = es.map(e=>interp(e,env))
        val addEnv = ps.zip(vals)
        interp(b, fenv() ++addEnv)
      }
      case ConstrV(n) =>{
        val vs = es.map(interp(_,env))
        VariantV(n,vs)
      }
      case v => error("not a function")
    }
    case If(c,t,e) => interp(c,env) match{
      case BoolV(true) => interp(t,env)
      case BoolV(false) => interp(e,env)
      case v => error("not a boolean")
    }
    case TypeDef(_,ws,body)=>{
      interp(body, env ++ ws.map(w=>w.name->ConstrV(w.name)))
    }
    case Match(expr, cases)=>interp(expr,env)match{
      case VariantV(wname, vs)=>cases.find(_.name == wname)match{
        case Some(MatchCase(_,ps,b))=>{
          if(ps.length!=vs.length){
            error()
          }
          interp(b,env ++(ps zip vs))
        }
        case None => error()
      }
      case _ => error()
    }
  }

  def mustValid(ty : Type,tenv : TypeEnv):Type = {ty match
    case NumT => NumT
    case BoolT => BoolT
    case ArrowT(ptys, rty) =>
      ArrowT(ptys.map(mustValid(_,tenv)),mustValid(rty,tenv))
    case NameT(tn)=>
      if(!tenv.tys.contains(tn)) error()
      NameT(tn)
    }
  def mustSame(lty : Type, rty : Type) : Unit ={
    if(lty!= rty){
      error()
    }
  }

  
}
