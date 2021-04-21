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
    | _ => raise brokenTypes;
end
