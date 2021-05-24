structure EVALUATOR  =
struct
open AST
val brokenTypes = Fail "Error in evaluation!"
val negateExc = Fail "Type error in Negation"
val BoolExc = Fail "Type error in Boolean evaluation"
val BinExc = Fail "Type error in Binary evaluation"
val CondExc = Fail "Type error in Conditional expression"
val AppExc = Fail "Type error in function Application"
val LambdaExc = Fail "Type error in Lambda function"

fun evalExp(e:exp, env:environment):value =
    case e of
	NumExp i              => IntVal i
    | StringExp s         => StringVal s
    | ConstExp c          => BoolVal c
    | VarExp x            => envLookup (x, env) 				  
    | BinExp (b, e1, e2)  => evalBinExp(b, e1, e2, env)
    | BoolExp (b, e1, e2)  => evalBoolExp(b, e1, e2, env)
    | LetExp(ValDecl(x, e1), e2)  =>
        let
            val v1 = evalExp (e1, env)
        in
            evalExp(e2, envAdd (x, v1, env))
        end	
    | LetExp(FunDecl(fnName, Lambda(arg, argType, retType, body)), e) => 
        evalExp(e, envAdd(fnName, LambdaVal(Lambda(arg,argType, retType, body)), env))


    | NegateExp(e) => 
        (case evalExp(e, env) of 
            IntVal(i) => IntVal(~i)
        |   BoolVal(b) => BoolVal(not b)
        | _ => raise negateExc)
        
    | ConditionalExp(e1, e2, e3) => 
        let 
            val BoolVal(b) = evalExp(e1, env)
        in 
            if b then evalExp(e2, env) else evalExp(e3, env)
        end
    | AppExp(e1, e2) =>
        let
            val LambdaVal(Lambda(arg,_,_, body)) = evalExp(e1, env)
            val v = evalExp(e2, env)
        in
            evalExp(body, envAdd(arg, v, env))
        end

    | LambdaExp(Lambda(arg,argType, retType, body)) =>  
        (case envLookupNoError(arg, env) of
    
            NULL => LambdaVal(Lambda(arg, argType, retType, substituteExp(body,env)))
            | _ => LambdaVal(Lambda(arg, argType, retType, body))
        )
    
    | DeclExp(ValDecl(id, e)) => evalExp(e, env)
    | DeclExp(FunDecl(id, f)) => LambdaVal(f) 

and
evalBinExp(b:binop, e1:exp, e2:exp, env:environment):value =
case (b, evalExp(e1, env), evalExp(e2, env))  of
    (Add, IntVal i1, IntVal i2) => IntVal (i1+i2)
  |   (Sub, IntVal i1, IntVal i2) => IntVal (i1-i2)
  |   (Mul, IntVal i1, IntVal i2) => IntVal (i1*i2)
  |   (Div, IntVal i1, IntVal i2) => IntVal (i1 div i2)
  |   _  => raise BinExc  					    
and
evalBoolExp(b:binop, e1:exp, e2:exp, env:environment):value =
case (b, evalExp(e1, env), evalExp(e2, env)) of
    (And, BoolVal(b1), BoolVal(b2)) => BoolVal(b1 andalso b2)
    |   (Or, BoolVal(b1), BoolVal(b2)) => BoolVal(b1 orelse b2)
    |   (Xor, BoolVal(b1), BoolVal(b2)) =>  if b1 <> b2 then BoolVal(true) else BoolVal(false)
    |   (Implies, BoolVal(b1), BoolVal(b2)) => BoolVal((not b1) orelse b2)
    |   (Eq, IntVal i1, IntVal i2)  => BoolVal (i1 = i2)
    |   (Eq, StringVal s1, StringVal s2) => BoolVal (s1 = s2)
    |   (Eq, BoolVal b1, BoolVal b2) => BoolVal (b1 = b2)
    |   (Greater, IntVal i1, IntVal i2)  => BoolVal (i1 > i2)
    |   (Greater, StringVal s1, StringVal s2) => BoolVal (s1 > s2)
    |   (Less, IntVal i1, IntVal i2)  => BoolVal (i1 < i2)
    |   (Less, StringVal s1, StringVal s2) => BoolVal (s1 < s2)
    | _ => raise BoolExc
and
substituteExp(e: exp, env: environment): exp =
    case e of 
    VarExp(v) => 
        (case envLookupNoError(v, env) of
            IntVal(i) => NumExp(i)
        |   StringVal(s) => StringExp(s)
        |   BoolVal(b) => ConstExp(b)
        |   LambdaVal(f) => LambdaExp(f)
        |   NULL => VarExp(v))
    | BinExp(b, e1, e2) => BinExp(b, substituteExp(e1, env), substituteExp(e2, env))
    | BoolExp(b, e1, e2) => BoolExp(b, substituteExp(e1, env), substituteExp(e2, env))
    | LetExp(ValDecl(x, e1), e2) => LetExp(ValDecl(x, substituteExp(e1, env)), substituteExp(e2,env))
    | LetExp(FunDecl(fnName, Lambda(arg, argType, retType, body)), e) => 
        let
            val LambdaExp(newLambda) = substituteExp(LambdaExp(Lambda(arg, argType, retType, body)), env)
            val newE = substituteExp(e, env)
        in
            (case envLookupNoError(fnName, env) of
            
                NULL => LetExp(FunDecl(fnName, newLambda), newE)
                | _ => LetExp(FunDecl(fnName, Lambda(arg, argType, retType, body)), e)
            )
          
        end
    | NegateExp(e) => NegateExp(substituteExp(e, env))
    | ConditionalExp(e1, e2, e3) => ConditionalExp(substituteExp(e1,env), substituteExp(e2,env), substituteExp(e3,env))
    | AppExp(e1, e2) => AppExp(substituteExp(e1, env), substituteExp(e2, env))
    | LambdaExp(Lambda(arg, argType, retType, body)) => 
        (case envLookupNoError(arg, env) of
    
            NULL => LambdaExp(Lambda(arg, argType, retType, substituteExp(body,env)))
            | _ => LambdaExp(Lambda(arg, argType, retType, body))
        )
    | NumExp(v) => NumExp(v)
    | ConstExp(v) => ConstExp(v)
    | StringExp(v) => StringExp(v)

and 
checkTypes(e: exp, typeEnv: typeEnvironment): compositeType =
    case e of 
        NumExp(n) => TYPE(INT)
    |   StringExp(s) => TYPE(STRING)
    |   ConstExp(c) => TYPE(BOOL)
    |   VarExp(v) => typeLookup(v, typeEnv)
    |   BinExp(b, e1, e2) => 
            let
              val t1 = checkTypes(e1, typeEnv)
              val t2 = checkTypes(e2, typeEnv)
            in
              if t1 = t2 andalso t1 = TYPE(INT) then t1 else raise BinExc
            end
    |   BoolExp(b, e1, e2) => 
            let
              val t1 = checkTypes(e1, typeEnv)
              val t2 = checkTypes(e2, typeEnv)
            in
              if t1 = t2 andalso (t1 = TYPE(INT) orelse t1 = TYPE(STRING) orelse t1 = TYPE(BOOL)) then 
                TYPE(BOOL) 
              else
                raise BoolExc
            end
    |   LetExp(ValDecl(x, e1), e2)  =>
            let
                val t1 = checkTypes (e1, typeEnv)
            in
                checkTypes(e2, typeEnvAdd (x, t1, typeEnv))
            end

    |   LetExp(FunDecl(fnName, Lambda(arg, argType, retType, body)), e) => 
            let
                val f = LambdaExp(Lambda(arg,argType, retType, body))
                val fnType = ARROW(argType, retType)
                val newTypeEnv = typeEnvAdd(fnName, fnType , typeEnv)
                val _ = checkTypes(f, newTypeEnv)
            in  
                checkTypes(e, newTypeEnv)
            end

    |   NegateExp(e) => checkTypes(e, typeEnv)
    |   ConditionalExp(e1, e2, e3) => 
            let
              val t1 = checkTypes(e1, typeEnv)
              val t2 = checkTypes(e2, typeEnv)
              val t3 = checkTypes(e3, typeEnv)
            in
              if t1 = TYPE(BOOL) andalso t2 = t3 then t2 else raise CondExc
            end
    |   AppExp(e1, e2) => 
            let
              val ARROW(t1, t2) = checkTypes(e1, typeEnv)
              val t3 = checkTypes(e2, typeEnv)
            in
              if t1 = t3 then t2 else raise AppExc
            end
    |   LambdaExp(Lambda(arg,argType, retType, body)) => 
            let
              val newTypeEnv = typeEnvAdd(arg, argType, typeEnv)
              val t1 = checkTypes(body, newTypeEnv)
            in
              if retType = t1 then ARROW(argType,retType) else raise LambdaExc
            end
    | DeclExp(ValDecl(id, e)) => checkTypes(e, typeEnv)
    | DeclExp(FunDecl(fnName, Lambda(arg, argType, retType, body))) => 
        let
          val f = LambdaExp(Lambda(arg,argType, retType, body))
          val fnType = ARROW(argType, retType)
          val newTypeEnv = typeEnvAdd(fnName, fnType , typeEnv)
          val _ = checkTypes(f, newTypeEnv)
        in
          fnType
        end
    
and 
evalWithTypeCheck(e:exp, env:environment, tenv: typeEnvironment): (value * compositeType) = 
    let
        val t = checkTypes(e, tenv)
        val v = evalExp(e, env)
    in
        (v , t)
    end
and    
evalProgram([], env, tenv) = []
| evalProgram(e::el, env, tenv): (value * compositeType) list = 
    let
      val (v,t) = evalWithTypeCheck(e, env, tenv)
    in
        ( case e of 
        DeclExp(d) => 
            (case d of
                ValDecl(id, _) => (v,t)::evalProgram(el, envAdd(id, v, env), typeEnvAdd(id, t, tenv))
            |   FunDecl(id, _ ) => (v,t)::evalProgram(el, envAdd(id, v, env), typeEnvAdd(id, t, tenv))
            )
            
        | _ => (v,t)::evalProgram(el, env, tenv)
    )
    end
    
end
