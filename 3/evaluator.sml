structure EVALUATOR  =
struct
open AST
val brokenTypes = Fail "Error in evaluation!"

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
    | LetExp(FunDecl(fnName, Lambda(arg, argType, retType, body)), e) => evalExp(e, envAdd(fnName, LambdaVal(Lambda(arg,argType, retType, body)), env))
    
    | NegateExp(e) => 
        (case evalExp(e, env) of 
            IntVal(i) => IntVal(~i)
        |   BoolVal(b) => BoolVal(not b)
        | _ => raise brokenTypes)
        
    | ConditionalExp(e1, e2, e3) => 
        let 
            val v1 = evalExp(e1, env)
            val v2 = evalExp(e2, env)
            val v3 = evalExp(e3, env)
            val valid = (case (v2, v3) of  
                (IntVal(i1), IntVal(i2)) => true
            |   (StringVal(s1), StringVal(s2)) => true
            |   (BoolVal(b1), BoolVal(b2)) => true
            | _ => raise brokenTypes)
            val BoolVal(b) = v1
        in 
            if b then v2 else v3
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
        
and
evalBinExp(b:binop, e1:exp, e2:exp, env:environment):value =
case (b, evalExp(e1, env), evalExp(e2, env))  of
    (Add, IntVal i1, IntVal i2) => IntVal (i1+i2)
  |   (Sub, IntVal i1, IntVal i2) => IntVal (i1-i2)
  |   (Mul, IntVal i1, IntVal i2) => IntVal (i1*i2)
  |   (Div, IntVal i1, IntVal i2) => IntVal (i1 div i2)
  |   _  => raise brokenTypes  					    
and
evalBoolExp(b:binop, e1:exp, e2:exp, env:environment):value =
case (b, evalExp(e1, env), evalExp(e2, env)) of
    (And, BoolVal(b1), BoolVal(b2)) => BoolVal(b1 andalso b2)
    |   (Or, BoolVal(b1), BoolVal(b2)) => BoolVal(b1 orelse b2)
    |   (Xor, BoolVal(b1), BoolVal(b2)) =>  if b1 <> b2 then BoolVal(true) else BoolVal(false)
    |   (Implies, BoolVal(b1), BoolVal(b2)) => BoolVal((not b1) orelse b2)
    |   (Eq, IntVal i1, IntVal i2)  => BoolVal (i1 = i2)
    |   (Eq, StringVal s1, StringVal s2) => BoolVal (s1 = s2)
    |   (Greater, IntVal i1, IntVal i2)  => BoolVal (i1 > i2)
    |   (Greater, StringVal s1, StringVal s2) => BoolVal (s1 > s2)
    |   (Less, IntVal i1, IntVal i2)  => BoolVal (i1 < i2)
    |   (Less, StringVal s1, StringVal s2) => BoolVal (s1 < s2)
    | _ => raise brokenTypes
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
end
