package kuplrg

object Implementation extends Template {

  import Expr.*
  import RecDef.*
  import Value.*
  import Type.*
  import TypeInfo.*

  def typeCheck(expr: Expr, tenv: TypeEnv): Type = expr match
    case EUnit => UnitT
    case ENum(_) => NumT
    case EBool(_) => BoolT
    case EStr(_)=> StrT
    case EId(x) =>{
      tenv.vars.getOrElse(x, error())
    }
    case EAdd(l,r)=>{
      mustSame(typeCheck(l,tenv),NumT)
      mustSame(typeCheck(r,tenv),NumT)
      NumT
    }
    case EMul(l,r)=>{
      mustSame(typeCheck(l,tenv),NumT)
      mustSame(typeCheck(r,tenv),NumT)
      NumT
    }
    case EDiv(l,r)=> {
      mustSame(typeCheck(l,tenv),NumT)
      mustSame(typeCheck(r,tenv),NumT)
      NumT
    }
    case EMod(l,r)=>{
      mustSame(typeCheck(l,tenv),NumT)
      mustSame(typeCheck(r,tenv),NumT)
      NumT
    }
    case EConcat(l,r)=>{
      mustSame(typeCheck(l,tenv),StrT)
      mustSame(typeCheck(r,tenv),StrT)
      StrT
    }
    case EEq(l,r) => {
      val lt = typeCheck(l,tenv)
      val rt = typeCheck(r,tenv)
      mustSame(lt,rt)
      BoolT
    }

    case ELt(l,r)=>{
      mustSame(typeCheck(l,tenv),NumT)
      mustSame(typeCheck(r,tenv),NumT)
      BoolT
    } 
    case ESeq(l,r) =>{
      val lt = typeCheck(l,tenv)
      val rt = typeCheck(r,tenv)
      rt
    }
    case EIf(c,te,ee) =>{
      mustSame(typeCheck(c,tenv),BoolT)
      val tt = typeCheck(te,tenv)
      val et = typeCheck(ee,tenv)
      mustSame(tt,et)
      tt
    }
    case EVal(x,tyOpt,init,body) =>{
      tyOpt match{
        case None => {
          val t1 = typeCheck(init,tenv)
          typeCheck(body, tenv.addVar(x -> t1))
        }
        case Some(t0)=>{
          val t1 = typeCheck(init,tenv)
          mustSame(t0,t1)
          typeCheck(body,tenv.addVar(x->t0))
        }
      }
    }
    case EFun(params, body) => {
      for(param<-params) mustValid(param.ty, tenv)
      ArrowT(Nil,params.map(p=>p.ty),typeCheck(body, tenv.addVars(params.map((param) => param.name -> param.ty))))
    }
    
    case EApp(fun, tys, args) => {
      typeCheck(fun, tenv) match {
        case ArrowT(names, ptys, rty) =>{
          for(ty <- tys) mustValid(ty,tenv)
          val argTypeList = args.map(typeCheck(_, tenv))
          val pairTypeList = argTypeList.zip(ptys)
          val subMap = names.zip(tys).toMap
          pairTypeList.foreach((argTy, pty) => mustSame(argTy, subst(pty, subMap)))
          subst(rty, subMap)
        }
        case ty => error()
      }
      
    }
    case ERecDefs(defs, body) => {
      val newtenv = defs.foldLeft(tenv){
        (tenv,d) => tenvUpdateRec(d,tenv)
      }
      defs.foreach((d) => (tRec(d,newtenv)))
      mustValid(typeCheck(body,newtenv),tenv)
    }
    case EMatch(expr, cs) => typeCheck(expr, tenv) match
      case IdT(name, tys) => tenv.tys.getOrElse(name, error()) match
        case TIVar => error()
        case TIAdt(tvars, variants)  =>
          val ts = cs.map(_.name).toSet
          val xs = variants.keySet
          if (ts != variants.keySet || ts.size != cs.length) error()
          val subMap: Map[String, Type] = tvars.zip(tys).toMap
          val caseList = cs.map(mcase => variants.find(vart => vart._1 == mcase.name) match {
            case Some(vart) => vart._2.map(param => param.ty)
          })
                                                  
          val tenvList = cs.map(_.params).zip(caseList).map((params, varTys) => params.zip(varTys.map(subst(_, subMap)))).map(tenv.addVars(_))
          val typeList = cs.map(mcase => mcase.body).zip(tenvList).map((body, newTEnv) =>{typeCheck(body,newTEnv)})
                                                      
          typeList.reduce((lt, rt) => { mustSame(lt, rt); lt })
      case _ => error()
    case EExit(ty, expr)                          => mustSame(typeCheck(expr, tenv), StrT); mustValid(ty, tenv)

  /*TYPE CHECKER HELPER FUNCTIONS FOR RECDEF*/
  def tenvUpdateRec(rdef : RecDef, tenv : TypeEnv) : TypeEnv = rdef match {
    case LazyVal(name, ty,init) =>{
      tenv.addVar((name,ty))
    }
    case RecFun(name, tvars, params, rty,body) => {
      tenv.addVar((name,ArrowT(tvars, params.map(_.ty),rty)))
    }
    case TypeDef(name, tvars,varts) => tenv.tys.get(name) match {
      case Some(_) => error()
      case None => 
      val newtenv = tenv.addTypeName(name, tvars, varts)
      varts.foldLeft(newtenv)((ntenv, vart) => {
          if (ntenv.vars.contains(vart.name)) error()
          ntenv.addVar((vart.name, ArrowT(tvars, vart.params.map(_.ty), IdT(name, tvars.map(tvar => IdT(tvar, Nil))))))
          
      })
    }
  }
  /*TYPE CHECKER HELPER FUNCTIONS FOR RECDEF*/
  def tRec(rdef : RecDef, tenv : TypeEnv) : Unit = rdef match {
    case LazyVal(name, ty, init) => {
      mustSame(mustValid(ty, tenv),typeCheck(init,tenv))
    }
    case RecFun(name, tvars, params, rty, body) =>{
      tvars.foreach(tvar => tenv.tys.get(tvar) match {
        case Some(_) => error()
        case None => ()
      })
      val newtenv = tenv.addTypeVars(tvars)
      params.foreach(param => mustValid(param.ty, newtenv))
      val newnewtenv = newtenv.addVars(params.map(param => (param.name, param.ty)))
      mustSame(typeCheck(body,newnewtenv), mustValid(rty,newtenv))
    }
    case TypeDef(name, tvars,varts) =>{
      tvars.foreach(tvar => tenv.tys.get(tvar) match {
        case Some(_) => error()
        case None => ()
      })
      val newtenv = tenv.addTypeVars(tvars)
      varts.foreach(vart => {vart.params.foreach(p => mustValid(p.ty,newtenv))})
    }
  }

  def interp(expr: Expr, env: Env): Value = expr match
    case EUnit => UnitV
    case ENum(n) => NumV(n)
    case EBool(b) => BoolV(b)
    case EStr(s) => StrV(s)
    case EId(x) => env.getOrElse(x,error()) match{
      case ExprV(expr, eenv) => interp(expr,eenv())
      case v => v
    }
    case EAdd(l,r) => (interp(l,env),interp(r,env)) match{
      case (NumV(l),NumV(r)) => NumV(l+r)
      case (l,r) => error()
    }
    case EMul(l,r) => (interp(l,env),interp(r,env)) match{
      case (NumV(l),NumV(r)) => NumV(l*r)
      case (l,r) => error()
    }
    case EDiv(l,r) => (interp(l,env),interp(r,env)) match{
      case (NumV(l),NumV(r)) => {
        if (NumV(r) != NumV(0)){
          NumV(l/r)
        }
        else{
          error()
        }
      }
      case (l,r) => error()
    }
    case EMod(l,r) => (interp(l,env),interp(r,env)) match{
      case (NumV(l),NumV(r)) => {
        if (NumV(r) != NumV(0)){
          NumV(l%r)
        }
        else{
          error()
        }
      }
      case (l,r) => error()
    }
    case EConcat(l,r) => (interp(l,env),interp(r,env)) match{
      case (StrV(l),StrV(r)) => StrV(l++r)
      case (l,r) => error()
    }
    case EEq(l,r) => BoolV(eq(interp(l, env), interp(r, env)))
    case ELt(l,r) => (interp(l,env),interp(r,env)) match{
      case (NumV(l),NumV(r)) => BoolV(l<r)
      case _ => error()
    }
    case ESeq(l,r) => {
      interp(l,env)
      interp(r,env)
    }
    case EIf(cond, te, ee) => interp(cond,env) match{
      case BoolV(true) => interp(te,env)
      case BoolV(false)=> interp(ee,env)
      case _ => error()
    }
    case EVal(x,tyOpt,init, body)=> {
      interp(body, env + (x -> interp(init, env)))
    }
    case EFun(params, body) => CloV(params.map(param => param.name), body, ()=>env)
    case EApp(fun, tys, args) => interp(fun, env) match
      case CloV(params, body, fenv) => interp(body, fenv() ++ params.zip(args.map(arg => interp(arg, env))).toMap)
      case ConstrV(name) => VariantV(name, args.map(arg => interp(arg, env)))
      case _  => error()
    case ERecDefs(defs, body) => {
      lazy val finalenv :Env = defs.foldLeft(env){
        (currentEnv,d) => envUpdateRec(currentEnv, () => finalenv,d)
      }
      interp(body, finalenv)
    }
    case EMatch(expr, mcases) => interp(expr,env) match{
      case VariantV(wname, vs) => mcases.find(_.name == wname) match{
        case Some(MatchCase(_,ps,b)) => {
          if(ps.length != vs.length) error()
          interp(b,env ++ (ps zip vs))
        }
        case None => error()
      }
      case v => error()
    }
    case _ => error()

  /*INTERPRETER HELPER FUNCTIONS FOR RECDEF*/
  def envUpdateRec(env1 : Env, env2 : () => Env, rdef:RecDef) : Env = rdef match{
    case LazyVal(name,ty,init)=>{
      env1+ (name->ExprV(init, env2))
    }
    case RecFun(name, tvars, params, rty,body) =>{
      env1 + (name -> CloV(params.map(param => param.name),body, env2))
    }
    case TypeDef(name, tvars,varts) =>{
      env1 ++ varts.map(vart => (vart.name -> ConstrV(vart.name))).toMap
    }
  }

  def mustValid(ty : Type,tenv : TypeEnv):Type = ty match{
    case UnitT => UnitT
    case NumT => NumT
    case BoolT =>BoolT
    case StrT => StrT
    case IdT(name,types) => tenv.tys.get(name) match{
      case Some(TIAdt(_,_)) =>
      {
        types.foreach(mustValid(_,tenv))
        IdT(name,types)
      }
      case Some(TIVar) => IdT(name, types)
      case _ => error()
    }
    case ArrowT(tvars, paramTys, retTy) =>{
      val newtenv = tenv.addTypeVars(tvars)
      for(paramTy <- paramTys) mustValid(paramTy,newtenv)
      ArrowT(tvars, paramTys, mustValid(retTy,newtenv))
    }
  }

  def subst(bodyTy:Type, subMap : Map[String, Type]):Type =bodyTy match{
    case UnitT => UnitT
    case NumT => NumT
    case BoolT => BoolT
    case StrT => StrT
    case IdT(x, tys) => tys match{
      case Nil => {
        subMap.getOrElse(x, IdT(x,tys))
      }
      case tys =>{
        IdT(x,tys.map(t=>subst(t, subMap)))
      }
    }
    case ArrowT(tvs, ptys, rty) => {
      ArrowT(tvs,ptys.map(pty => subst(pty,subMap.filter(m => !tvs.contains(m._1)))),subst(rty,subMap.filter(m => !tvs.contains(m._1))))
    }
  }

  def isSame(lty : Type, rty : Type) : Boolean = (lty, rty) match{
    case (NumT, NumT) => true
    case (BoolT, BoolT) => true
    case (UnitT, UnitT) => true
    case (StrT, StrT) => true
    case (IdT(name1, tys1), IdT(name2, tys2)) => (tys1, tys2) match{
      case (Nil, Nil) => true
      case (h1::t1,h2::t2) => {
        tys1.zip(tys2).map((lt,rt) => isSame(lt,rt)).foldLeft(true)(_ && _)
      }
    }
    case (ArrowT(tvars1, paramTys1, retTy1),ArrowT(tvars2,paramTys2,retTy2)) =>{
      if(tvars1.length == tvars2.length && paramTys1.length == paramTys2.length){
        
        val subMap = tvars2.zip(tvars1.map(tvar1 => IdT(tvar1,Nil))).toMap
        (paramTys1.zip(paramTys2).toMap + (retTy1 -> retTy2)).map((lt,rt) => isSame(lt,subst(rt,subMap))).foldLeft(true)(_ && _)
      }
      else false
    }

    case _ => false
  }

  def mustSame(lty : Type, rty : Type) : Unit ={
    if(!isSame(lty,rty)){
      error()
    }
  }

  def eq(left: Value, right: Value): Boolean = (left, right) match
    case (UnitV, UnitV) => true
    case (NumV(l), NumV(r)) => l == r
    case (BoolV(l), BoolV(r)) => l == r
    case (StrV(l), StrV(r)) => l == r
    case (VariantV(lname, lvalues), VariantV(rname, rvalues)) => {
      lvalues.zip(rvalues).map((lvalue, rvalue) => eq(lvalue, rvalue)).foldLeft(lname == rname)(_ && _)
    }
    case (_,_)=> false

}