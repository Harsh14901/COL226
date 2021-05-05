structure AST =
struct

type id = string

datatype binop = Add | Sub | Mul | Div | Eq | Less | Greater
| And | Or | Xor | Implies

datatype decl = ValDecl of id * exp
							| FunDecl of id * function

and exp = NumExp of int
    	| StringExp of string
			| ConstExp of bool
    	| VarExp of id
			| BinExp of binop * exp * exp
      | BoolExp of binop * exp * exp
			| LetExp of decl * exp
			| NegateExp of exp
			| ConditionalExp of exp * exp * exp
			| AppExp of exp * exp
			| LambdaExp of function
and  
function = Lambda of id * compositeType * compositeType * exp
and
value = IntVal of int
              | StringVal of string
	       			| BoolVal of bool
							| LambdaVal of function
							| NULL
and
primitiveType = INT | BOOL | STRING | REAL

and
compositeType = ARROW of compositeType * compositeType
							| TYPE of primitiveType
									

type environment = (id * value) list

type typeEnvironment = (id * compositeType) list

fun envAdd (var:id, v:value, env:environment) =
    (var,v)::env

fun typeEnvAdd (var:id, t:compositeType, env:typeEnvironment) =
    (var,t)::env

fun envLookup (var:id, env:environment) =
    case List.find(fn (x, _) => x = var) env of
				       SOME (x, v)   => v
				    |   NONE => raise Fail "Environment lookup error"							    

fun typeLookup (var:id, env:typeEnvironment) =
    case List.find(fn (x, _) => x = var) env of
				       SOME (x, v)   => v
				    |   NONE => raise Fail "Environment lookup error"							    

fun envLookupNoError (var:id, env:environment) =
    case List.find(fn (x, _) => x = var) env of
				       SOME (x, v)   => v
				    |   NONE => NULL

end
			


							    
