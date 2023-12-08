package kuplrg

object Implementation extends Template {

  import Expr.*
  import RecDef.*
  import Value.*
  import Type.*
  import TypeInfo.*

  def typeCheck(expr: Expr, tenv: TypeEnv): Type = expr match{
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
      val ptys = params.map(_.ty)
      for(pty<-ptys) mustValid(pty, tenv)
      val rty = typeCheck(body, tenv.addVars(params.map(p => p.name -> p.ty)))
      ArrowT(List.empty[String],ptys, rty)
    }
    

    //todo : subst반영해야함
    case EApp(fun, tys, args) => {
      for(ty <- tys) mustValid(ty,tenv)
      typeCheck(fun, tenv) match {
        case ArrowT(names, ptys, rty) =>{
          if(ptys.length != args.length) error()
          for(n<-args.indices) {
            typeCheck(args(n),tenv)
            mustSame(typeCheck(args(n),tenv),ptys(n))
          }
          rty
        }
        case ty => error()
      }
      
    }
    case ERecDefs(defs, body) => {
      val newtenv = defs.foldLeft(tenv){
        (currentEnv,d) => tenvUpdateRec(d,currentEnv)
      }
      defs.foreach(d => tRec(d,newtenv))
      mustValid(typeCheck(body,newtenv),newtenv)
      typeCheck(body, newtenv)
    }

    case EMatch(e, mcases) => ???

    case EExit(ty,expr) =>{
      mustValid(ty, tenv)
      mustSame(typeCheck(expr, tenv),StrT)
      ty
      
    }
    
  }
  def interp(expr: Expr, env: Env): Value = expr match{
    case EUnit => UnitV
    case ENum(n) => NumV(n)
    case EBool(b) => BoolV(b)
    case EStr(s) => StrV(s)
    case EId(x) => {
      env(x) match{
        case ExprV(e,newEnv) =>{
          val newnewEnv : Env = newEnv()
          interp(e,newnewEnv)
        }
        case _ => env.getOrElse(x,error())
      }
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
    case ESeq(l,r) => interp(r,env)
    case EIf(cond, te, ee) => interp(cond,env) match{
      case BoolV(true) => interp(te,env)
      case BoolV(false)=> interp(ee,env)
    }
    case EVal(x,tyOpt,init, body)=> {
      tyOpt match{
        case None => interp(body,env + (x -> interp(init, env)))
        case Some(t)=> interp(body, env + (x -> interp(init, env)))
      }
    }
    case EFun(params, body) => CloV(params.map(param => param.name), body, ()=>env)
    case EApp(fun,tys, args) => interp(fun,env) match {
      case CloV(ps,b,fenv) => {
        val vals = args.map(e=>interp(e,env))
        val addEnv = ps.zip(vals)
        interp(b, fenv() ++addEnv)
      }
      case ConstrV(n) =>{
        val vs = args.map(interp(_,env))
        VariantV(n,vs)
      }
      case v => error()
    }
    case ERecDefs(defs, body) => {
      val finalenv : Env = Map()
      val newenv = defs.foldLeft(env){
        (currentEnv,d) => envUpdateRec(currentEnv,finalenv,d)
      }
      interp(body, newenv)
    }

    case EExit(ty, expr)=>{
      error()
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
  }

  def eq(left: Value, right: Value): Boolean = (left, right) match
    case (UnitV, UnitV) => true
    case (NumV(l), NumV(r)) => l == r
    case (BoolV(l), BoolV(r)) => l == r
    case (StrV(l), StrV(r)) => l == r
    case (VariantV(lname, lvalues), VariantV(rname, rvalues)) => {
      if(lname != rname){
        lvalues.zip(rvalues).forall { case (l, r) => eq(l,r) }
      }
      else false
    }
    case _=> false
  
  //얘는 다 된 듯?
  def mustValid(ty : Type,tenv : TypeEnv):Type = ty match{
    case UnitT => UnitT
    case NumT => NumT
    case BoolT =>BoolT
    case StrT => StrT
    case IdT(name,types) => types match{
      case Nil =>{
        if(!tenv.vars.contains(name)) error()
        IdT(name,types)
      }
      case h::t => {
        tenv.tys(name) match 
        case TIAdt(tvars,variants)=>{
          for (ty <- types) mustValid(ty,tenv)
          IdT(name,types)
        }
        case _ => error()
      }
    }
    case ArrowT(tvars, paramTys, retTy) =>{
      val newtenv = tenv.addTypeVars(tvars)
      for(paramTy <- paramTys) mustValid(paramTy,newtenv)
      ArrowT(tvars, paramTys, mustValid(retTy,newtenv))
    }
  }
  
  //tys,paramTys 다 리스트인데 어떻게 처리해야하노 -> substAll도입?
  def subst(bodyTy:Type, name:String,ty:Type):Type =bodyTy match{
    case UnitT => UnitT
    case NumT => NumT
    case BoolT => BoolT
    case StrT => StrT
    case IdT(x, tys) => tys match{
      case Nil => {
        if(x == name) ty
        else IdT(x,tys)
      }
      case (h::t) =>{
        if(x == name) IdT(x,tys)
        else IdT(x,tys.map(ty=>subst(bodyTy, name, ty)))
      }
    }
    
  }
  // def substAll(bodyTy : Type, name : String, ty : Type) : Type = bodyTy match{
  //   case ArrowT(tvars, paramTys, retTy) =>{

  //   }
  //   case _ => error()
  // }
  

   //todo: ArrowT에 subst 반영
  def isSame(lty : Type, rty : Type) : Boolean = (lty, rty) match{
    case (NumT, NumT) => true
    case (BoolT, BoolT) => true
    case (UnitT, UnitT) => true
    case (StrT, StrT) => true
    case (IdT(name1, tys1), IdT(name2, tys2)) => (tys1, tys2) match{
      case (Nil, Nil) => true
      case (h1::t1,h2::t2) => {
        tys1.zip(tys2).forall{case (lt,rt) => isSame(lt,rt)}
      }
    }
    case (ArrowT(tvars1, paramTys1, retTy1),ArrowT(tvars2,paramTys2,retTy2)) =>{
      paramTys1.zip(paramTys2).forall { case (lt, rt) => {
        isSame(lt,rt)
        // isSame(lt,subst(rt, tvars2(0), IdT(tvars1(0))))
      } } //todo: subst반영해야함
      isSame(retTy1, retTy2)
    }

    case _ => false
  }

  def mustSame(lty : Type, rty : Type) : Unit ={
    if(!isSame(lty,rty)){
      error()
    }
  }

  def tenvUpdateRec(rdef : RecDef, tenv : TypeEnv) : TypeEnv = rdef match {
    case LazyVal(name, ty,init) =>{
      tenv.addVar(name -> ty)
    }
    case RecFun(name, tvars, params, rty,body) => {
      tenv.addVar(name -> ArrowT(tvars, params.map(param => param.ty),rty))
    }
    case TypeDef(name, tvars,varts) => {
      if(!tenv.vars.contains(name)){
        val newtenv = TypeEnv(Map(),Map())
        newtenv.addTypeName(name, tvars, varts)
        for(n<-varts.indices){
          newtenv.addVar(varts(n).name -> ArrowT(tvars, varts(n).params.map(param => param.ty),IdT(name, varts(n).params.map(param => param.ty))))
        }//todo : 마지막 인자 varts(n)아닌듯
        newtenv
      }
      else error()
    }
  }

  def envUpdateRec(env1 : Env, env2: Env, rdef:RecDef) : Env = rdef match{
    case LazyVal(name,ty,init)=>{
      env1 + (name->ExprV(init, ()=>env2))
    }
    case RecFun(name, tvars, params, rty,body) =>{
      env1 + (name -> CloV(params.map(param => param.name),body, ()=>env2))
    }
    case TypeDef(name, tvars,varts) =>{
      varts.foldLeft(env1){
        (currentEnv, vart) => currentEnv + (vart.name -> ConstrV(vart.name))
      }
    }
  }

  def tRec(rdef : RecDef, tenv : TypeEnv) : RecDef = rdef match {
    case LazyVal(name, ty, init) => {
      mustValid(ty, tenv)
      mustSame(ty,typeCheck(init,tenv))
      LazyVal(name, ty,init)
    }
    case RecFun(name, tvars, params, rty, body) =>{
      val allNotContained: Boolean = tvars.forall(tv => !tenv.vars.contains(tv))
      if (allNotContained){
        val newtenv = tenv.addTypeVars(tvars)
        params.foreach(param => mustValid(param.ty, tenv))
        val newnewtenv = tenv.addVars(params.map(param => (param.name, param.ty)))
        mustSame(typeCheck(body,newnewtenv), rty)
        RecFun(name, tvars, params, rty, body)
      }
      else error()
    }
    case TypeDef(name, tvars,varts) =>{
      val allNotContained: Boolean = tvars.forall(tv => !tenv.vars.contains(tv))
      if (allNotContained){
        val newtenv = tenv.addTypeVars(tvars)
        varts.foreach(vart => {vart.params.foreach(p => mustValid(p.ty,newtenv))})
        TypeDef(name, tvars,varts)
      }
      else error()
    }
  }

  
}
