functor CalcLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Calc_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(*#line 1.2 "parser.yacc"*)
(* Some user decl here *)


(*#line 15.1 "parser.yacc.sml"*)
end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\002\000\000\000\000\000\
\\001\000\002\000\029\000\004\000\028\000\005\000\027\000\006\000\026\000\
\\007\000\025\000\009\000\024\000\016\000\023\000\017\000\022\000\
\\018\000\021\000\019\000\020\000\021\000\019\000\022\000\018\000\000\000\
\\001\000\003\000\015\000\004\000\028\000\005\000\027\000\006\000\026\000\
\\007\000\025\000\009\000\024\000\010\000\014\000\014\000\013\000\
\\015\000\056\000\016\000\023\000\017\000\022\000\018\000\021\000\
\\019\000\020\000\020\000\012\000\021\000\019\000\022\000\018\000\
\\023\000\011\000\027\000\010\000\028\000\009\000\036\000\008\000\
\\037\000\007\000\038\000\006\000\000\000\
\\001\000\003\000\015\000\010\000\014\000\014\000\013\000\020\000\012\000\
\\023\000\011\000\027\000\010\000\028\000\009\000\036\000\008\000\
\\037\000\007\000\038\000\006\000\000\000\
\\001\000\004\000\028\000\005\000\027\000\006\000\026\000\007\000\025\000\
\\009\000\024\000\011\000\057\000\016\000\023\000\017\000\022\000\
\\018\000\021\000\019\000\020\000\021\000\019\000\022\000\018\000\000\000\
\\001\000\004\000\028\000\005\000\027\000\006\000\026\000\007\000\025\000\
\\009\000\024\000\012\000\072\000\016\000\023\000\017\000\022\000\
\\018\000\021\000\019\000\020\000\021\000\019\000\022\000\018\000\000\000\
\\001\000\004\000\028\000\005\000\027\000\006\000\026\000\007\000\025\000\
\\009\000\024\000\013\000\082\000\016\000\023\000\017\000\022\000\
\\018\000\021\000\019\000\020\000\021\000\019\000\022\000\018\000\000\000\
\\001\000\004\000\028\000\005\000\027\000\006\000\026\000\007\000\025\000\
\\009\000\024\000\015\000\061\000\016\000\023\000\017\000\022\000\
\\018\000\021\000\019\000\020\000\021\000\019\000\022\000\018\000\000\000\
\\001\000\004\000\028\000\005\000\027\000\006\000\026\000\007\000\025\000\
\\009\000\024\000\016\000\023\000\017\000\022\000\018\000\021\000\
\\019\000\020\000\021\000\019\000\022\000\018\000\025\000\071\000\000\000\
\\001\000\008\000\030\000\000\000\
\\001\000\014\000\032\000\000\000\
\\001\000\014\000\052\000\000\000\
\\001\000\014\000\070\000\032\000\069\000\033\000\068\000\034\000\067\000\
\\035\000\066\000\000\000\
\\001\000\015\000\075\000\030\000\074\000\000\000\
\\001\000\015\000\078\000\030\000\074\000\000\000\
\\001\000\015\000\081\000\030\000\074\000\000\000\
\\001\000\024\000\054\000\000\000\
\\001\000\028\000\009\000\036\000\034\000\000\000\
\\001\000\029\000\059\000\000\000\
\\001\000\029\000\063\000\000\000\
\\001\000\029\000\080\000\000\000\
\\001\000\029\000\083\000\000\000\
\\001\000\030\000\074\000\031\000\086\000\000\000\
\\001\000\030\000\074\000\031\000\087\000\000\000\
\\001\000\036\000\031\000\000\000\
\\001\000\036\000\053\000\000\000\
\\001\000\036\000\058\000\000\000\
\\091\000\000\000\
\\092\000\003\000\015\000\010\000\014\000\014\000\013\000\020\000\012\000\
\\023\000\011\000\027\000\010\000\028\000\009\000\036\000\008\000\
\\037\000\007\000\038\000\006\000\000\000\
\\093\000\000\000\
\\094\000\000\000\
\\095\000\004\000\028\000\005\000\027\000\006\000\026\000\007\000\025\000\
\\009\000\024\000\016\000\023\000\017\000\022\000\018\000\021\000\
\\019\000\020\000\021\000\019\000\022\000\018\000\000\000\
\\096\000\004\000\028\000\005\000\027\000\006\000\026\000\007\000\025\000\
\\009\000\024\000\016\000\023\000\017\000\022\000\018\000\021\000\
\\019\000\020\000\021\000\019\000\022\000\018\000\000\000\
\\097\000\000\000\
\\098\000\008\000\030\000\000\000\
\\099\000\000\000\
\\100\000\016\000\023\000\017\000\022\000\018\000\021\000\019\000\020\000\
\\021\000\019\000\022\000\018\000\000\000\
\\101\000\000\000\
\\102\000\016\000\023\000\017\000\022\000\018\000\021\000\019\000\020\000\
\\021\000\019\000\022\000\018\000\000\000\
\\103\000\016\000\023\000\017\000\022\000\018\000\021\000\019\000\020\000\
\\021\000\019\000\022\000\018\000\000\000\
\\104\000\016\000\023\000\017\000\022\000\018\000\021\000\019\000\020\000\
\\021\000\019\000\022\000\018\000\000\000\
\\105\000\016\000\023\000\017\000\022\000\018\000\021\000\019\000\020\000\
\\021\000\019\000\022\000\018\000\000\000\
\\106\000\004\000\028\000\005\000\027\000\006\000\026\000\007\000\025\000\
\\009\000\024\000\016\000\023\000\017\000\022\000\018\000\021\000\
\\019\000\020\000\021\000\019\000\022\000\018\000\000\000\
\\107\000\018\000\021\000\019\000\020\000\000\000\
\\108\000\018\000\021\000\019\000\020\000\000\000\
\\109\000\000\000\
\\110\000\000\000\
\\111\000\016\000\023\000\017\000\022\000\018\000\021\000\019\000\020\000\000\000\
\\112\000\016\000\023\000\017\000\022\000\018\000\021\000\019\000\020\000\000\000\
\\113\000\000\000\
\\114\000\000\000\
\\115\000\000\000\
\\116\000\004\000\028\000\005\000\027\000\006\000\026\000\007\000\025\000\
\\009\000\024\000\016\000\023\000\017\000\022\000\018\000\021\000\
\\019\000\020\000\021\000\019\000\022\000\018\000\000\000\
\\117\000\000\000\
\\118\000\000\000\
\\119\000\030\000\074\000\000\000\
\\120\000\000\000\
\\121\000\000\000\
\\122\000\000\000\
\\123\000\000\000\
\\124\000\000\000\
\\125\000\000\000\
\"
val actionRowNumbers =
"\003\000\054\000\028\000\001\000\
\\035\000\033\000\034\000\024\000\
\\010\000\017\000\003\000\003\000\
\\003\000\003\000\028\000\027\000\
\\003\000\003\000\003\000\003\000\
\\003\000\003\000\003\000\003\000\
\\003\000\003\000\003\000\030\000\
\\003\000\011\000\025\000\016\000\
\\009\000\037\000\002\000\004\000\
\\036\000\029\000\047\000\048\000\
\\045\000\046\000\044\000\043\000\
\\042\000\041\000\040\000\039\000\
\\038\000\031\000\026\000\018\000\
\\003\000\007\000\051\000\003\000\
\\019\000\012\000\008\000\053\000\
\\005\000\012\000\013\000\056\000\
\\060\000\061\000\059\000\058\000\
\\012\000\049\000\003\000\014\000\
\\012\000\020\000\015\000\006\000\
\\021\000\055\000\012\000\057\000\
\\050\000\012\000\022\000\023\000\
\\003\000\003\000\052\000\032\000\
\\000\000"
val gotoT =
"\
\\001\000\003\000\002\000\088\000\004\000\002\000\005\000\001\000\000\000\
\\000\000\
\\001\000\003\000\003\000\015\000\004\000\014\000\005\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\031\000\000\000\
\\001\000\033\000\005\000\001\000\000\000\
\\001\000\034\000\005\000\001\000\000\000\
\\001\000\035\000\005\000\001\000\000\000\
\\001\000\036\000\005\000\001\000\000\000\
\\001\000\003\000\003\000\037\000\004\000\014\000\005\000\001\000\000\000\
\\000\000\
\\001\000\038\000\005\000\001\000\000\000\
\\001\000\039\000\005\000\001\000\000\000\
\\001\000\040\000\005\000\001\000\000\000\
\\001\000\041\000\005\000\001\000\000\000\
\\001\000\042\000\005\000\001\000\000\000\
\\001\000\043\000\005\000\001\000\000\000\
\\001\000\044\000\005\000\001\000\000\000\
\\001\000\045\000\005\000\001\000\000\000\
\\001\000\046\000\005\000\001\000\000\000\
\\001\000\047\000\005\000\001\000\000\000\
\\001\000\048\000\005\000\001\000\000\000\
\\000\000\
\\001\000\049\000\005\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\053\000\005\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\058\000\005\000\001\000\000\000\
\\000\000\
\\000\000\
\\001\000\060\000\005\000\001\000\000\000\
\\000\000\
\\006\000\063\000\007\000\062\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\063\000\007\000\071\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\063\000\007\000\074\000\000\000\
\\000\000\
\\001\000\075\000\005\000\001\000\000\000\
\\000\000\
\\006\000\063\000\007\000\077\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\063\000\007\000\082\000\000\000\
\\000\000\
\\000\000\
\\006\000\063\000\007\000\083\000\000\000\
\\000\000\
\\000\000\
\\001\000\086\000\005\000\001\000\000\000\
\\001\000\087\000\005\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 89
val numrules = 35
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit | NUM of unit ->  (int) | CONST of unit ->  (bool) | ID of unit ->  (string) | compositeType of unit ->  (AST.compositeType) | primitiveType of unit ->  (AST.primitiveType) | DECL of unit ->  (AST.decl) | Statement of unit ->  (AST.exp) | StatementSet of unit ->  ( ( AST.exp )  list) | Program of unit ->  ( ( AST.exp )  list) | EXP of unit ->  (AST.exp)
end
type svalue = MlyValue.svalue
type result =  ( AST.exp )  list
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "TERM"
  | (T 2) => "NOT"
  | (T 3) => "AND"
  | (T 4) => "OR"
  | (T 5) => "XOR"
  | (T 6) => "EQUALS"
  | (T 7) => "ASSIGN"
  | (T 8) => "IMPLIES"
  | (T 9) => "IF"
  | (T 10) => "THEN"
  | (T 11) => "ELSE"
  | (T 12) => "FI"
  | (T 13) => "LPAREN"
  | (T 14) => "RPAREN"
  | (T 15) => "PLUS"
  | (T 16) => "MINUS"
  | (T 17) => "DIV"
  | (T 18) => "TIMES"
  | (T 19) => "NEGATE"
  | (T 20) => "LESSTHAN"
  | (T 21) => "GREATERTHAN"
  | (T 22) => "LET"
  | (T 23) => "IN"
  | (T 24) => "END"
  | (T 25) => "VAL"
  | (T 26) => "FN"
  | (T 27) => "FUN"
  | (T 28) => "COLON"
  | (T 29) => "ARROW"
  | (T 30) => "DECLARROW"
  | (T 31) => "INT"
  | (T 32) => "BOOL"
  | (T 33) => "STRING"
  | (T 34) => "REAL"
  | (T 35) => "ID"
  | (T 36) => "CONST"
  | (T 37) => "NUM"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.StatementSet StatementSet1, _, StatementSet1right)) :: ( _, ( MlyValue.Statement Statement1, Statement1left, _)) :: rest671)) => let val  result = MlyValue.Program (fn _ => let val  (Statement as Statement1) = Statement1 ()
 val  (StatementSet as StatementSet1) = StatementSet1 ()
 in ((*#line 73.44 "parser.yacc"*)Statement::StatementSet(*#line 369.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 1, ( result, Statement1left, StatementSet1right), rest671)
end
|  ( 1, ( rest671)) => let val  result = MlyValue.StatementSet (fn _ => ((*#line 74.44 "parser.yacc"*)[](*#line 376.1 "parser.yacc.sml"*)
))
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 2, ( ( _, ( MlyValue.StatementSet StatementSet1, _, StatementSet1right)) :: ( _, ( MlyValue.Statement Statement1, Statement1left, _)) :: rest671)) => let val  result = MlyValue.StatementSet (fn _ => let val  (Statement as Statement1) = Statement1 ()
 val  (StatementSet as StatementSet1) = StatementSet1 ()
 in ((*#line 75.44 "parser.yacc"*)Statement::StatementSet(*#line 380.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, Statement1left, StatementSet1right), rest671)
end
|  ( 3, ( ( _, ( _, _, TERM1right)) :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.Statement (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in ((*#line 76.44 "parser.yacc"*)EXP(*#line 387.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 3, ( result, EXP1left, TERM1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = MlyValue.DECL (fn _ => let val  (ID as ID1) = ID1 ()
 val  (EXP as EXP1) = EXP1 ()
 in ((*#line 78.28 "parser.yacc"*)AST.ValDecl(ID, EXP)(*#line 393.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 4, ( result, ID1left, EXP1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( MlyValue.compositeType compositeType2, _, _)) :: _ :: _ :: ( _, ( MlyValue.compositeType compositeType1, _, _)) :: _ :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, FUN1left, _)) :: rest671)) => let val  result = MlyValue.DECL (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  compositeType1 = compositeType1 ()
 val  compositeType2 = compositeType2 ()
 val  (EXP as EXP1) = EXP1 ()
 in ((*#line 79.84 "parser.yacc"*)AST.FunDecl(ID1, AST.Lambda(ID2, compositeType1, compositeType2, EXP))(*#line 400.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 4, ( result, FUN1left, EXP1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.CONST CONST1, CONST1left, CONST1right)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (CONST as CONST1) = CONST1 ()
 in ((*#line 82.32 "parser.yacc"*)AST.ConstExp(CONST)(*#line 410.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, CONST1left, CONST1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (ID as ID1) = ID1 ()
 in ((*#line 83.32 "parser.yacc"*)AST.VarExp(ID)(*#line 416.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, ID1left, ID1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (NUM as NUM1) = NUM1 ()
 in ((*#line 84.32 "parser.yacc"*)AST.NumExp(NUM)(*#line 422.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, NUM1left, NUM1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: ( _, ( _, NOT1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in ((*#line 85.32 "parser.yacc"*)AST.NegateExp(EXP)(*#line 428.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, NOT1left, EXP1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: ( _, ( _, NEGATE1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in ((*#line 86.32 "parser.yacc"*)AST.NegateExp(EXP)(*#line 434.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, NEGATE1left, EXP1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 87.32 "parser.yacc"*)AST.BoolExp(AST.And, EXP1, EXP2)(*#line 440.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 88.32 "parser.yacc"*)AST.BoolExp(AST.Or, EXP1, EXP2)(*#line 447.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 89.32 "parser.yacc"*)AST.BoolExp(AST.Xor, EXP1, EXP2)(*#line 454.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 90.32 "parser.yacc"*)AST.BoolExp(AST.Eq, EXP1, EXP2)(*#line 461.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 91.32 "parser.yacc"*)AST.BoolExp(AST.Implies, EXP1, EXP2)(*#line 468.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 92.32 "parser.yacc"*)AST.BinExp(AST.Add, EXP1,  EXP2)(*#line 475.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 93.32 "parser.yacc"*)AST.BinExp(AST.Sub,  EXP1,  EXP2)(*#line 482.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 94.32 "parser.yacc"*)AST.BinExp(AST.Mul,  EXP1, EXP2)(*#line 489.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 95.32 "parser.yacc"*)AST.BinExp(AST.Div, EXP1, EXP2)(*#line 496.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 96.32 "parser.yacc"*)AST.BoolExp(AST.Greater, EXP1, EXP2)(*#line 503.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 97.32 "parser.yacc"*)AST.BoolExp(AST.Less, EXP1, EXP2)(*#line 510.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 22, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.EXP EXP1, _, _)) :: _ :: ( _, ( MlyValue.DECL DECL1, _, _)) :: ( _, ( _, LET1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (DECL as DECL1) = DECL1 ()
 val  (EXP as EXP1) = EXP1 ()
 in ((*#line 98.32 "parser.yacc"*)AST.LetExp(DECL, EXP)(*#line 517.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, LET1left, END1right), rest671)
end
|  ( 23, ( ( _, ( _, _, FI1right)) :: ( _, ( MlyValue.EXP EXP3, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 val  EXP3 = EXP3 ()
 in ((*#line 99.32 "parser.yacc"*)AST.ConditionalExp(EXP1, EXP2, EXP3)(*#line 524.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, IF1left, FI1right), rest671)
end
|  ( 24, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in ((*#line 100.32 "parser.yacc"*)EXP(*#line 532.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( MlyValue.compositeType compositeType2, _, _)) :: _ :: _ :: ( _, ( MlyValue.compositeType compositeType1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: _ :: ( _, ( _, FN1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (ID as ID1) = ID1 ()
 val  compositeType1 = compositeType1 ()
 val  compositeType2 = compositeType2 ()
 val  (EXP as EXP1) = EXP1 ()
 in ((*#line 101.81 "parser.yacc"*)AST.LambdaExp(AST.Lambda(ID, compositeType1, compositeType2, EXP))(*#line 538.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, FN1left, EXP1right), rest671)
end
|  ( 26, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP2, _, _)) :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 102.33 "parser.yacc"*)AST.AppExp(EXP1, EXP2)(*#line 547.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.DECL DECL1, DECL1left, DECL1right)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (DECL as DECL1) = DECL1 ()
 in ((*#line 103.33 "parser.yacc"*)AST.DeclExp(DECL)(*#line 554.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, DECL1left, DECL1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.compositeType compositeType2, _, compositeType2right)) :: _ :: ( _, ( MlyValue.compositeType compositeType1, compositeType1left, _)) :: rest671)) => let val  result = MlyValue.compositeType (fn _ => let val  compositeType1 = compositeType1 ()
 val  compositeType2 = compositeType2 ()
 in ((*#line 106.38 "parser.yacc"*)AST.ARROW(compositeType1, compositeType2)(*#line 560.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 6, ( result, compositeType1left, compositeType2right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.primitiveType primitiveType1, primitiveType1left, primitiveType1right)) :: rest671)) => let val  result = MlyValue.compositeType (fn _ => let val  (primitiveType as primitiveType1) = primitiveType1 ()
 in ((*#line 107.34 "parser.yacc"*)AST.TYPE(primitiveType)(*#line 567.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 6, ( result, primitiveType1left, primitiveType1right), rest671)
end
|  ( 30, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.compositeType compositeType1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.compositeType (fn _ => let val  (compositeType as compositeType1) = compositeType1 ()
 in ((*#line 108.34 "parser.yacc"*)compositeType(*#line 573.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 6, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 31, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  result = MlyValue.primitiveType (fn _ => ((*#line 109.26 "parser.yacc"*)AST.INT(*#line 579.1 "parser.yacc.sml"*)
))
 in ( LrTable.NT 5, ( result, INT1left, INT1right), rest671)
end
|  ( 32, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  result = MlyValue.primitiveType (fn _ => ((*#line 110.26 "parser.yacc"*)AST.BOOL(*#line 583.1 "parser.yacc.sml"*)
))
 in ( LrTable.NT 5, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 33, ( ( _, ( _, REAL1left, REAL1right)) :: rest671)) => let val  result = MlyValue.primitiveType (fn _ => ((*#line 111.26 "parser.yacc"*)AST.REAL(*#line 587.1 "parser.yacc.sml"*)
))
 in ( LrTable.NT 5, ( result, REAL1left, REAL1right), rest671)
end
|  ( 34, ( ( _, ( _, STRING1left, STRING1right)) :: rest671)) => let val  result = MlyValue.primitiveType (fn _ => ((*#line 112.26 "parser.yacc"*)AST.STRING(*#line 591.1 "parser.yacc.sml"*)
))
 in ( LrTable.NT 5, ( result, STRING1left, STRING1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.Program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Calc_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(ParserData.MlyValue.VOID,p1,p2))
fun TERM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(ParserData.MlyValue.VOID,p1,p2))
fun XOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(ParserData.MlyValue.VOID,p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(ParserData.MlyValue.VOID,p1,p2))
fun IMPLIES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(ParserData.MlyValue.VOID,p1,p2))
fun FI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(ParserData.MlyValue.VOID,p1,p2))
fun NEGATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(ParserData.MlyValue.VOID,p1,p2))
fun LESSTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(ParserData.MlyValue.VOID,p1,p2))
fun GREATERTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(ParserData.MlyValue.VOID,p1,p2))
fun VAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(ParserData.MlyValue.VOID,p1,p2))
fun FN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(ParserData.MlyValue.VOID,p1,p2))
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(ParserData.MlyValue.VOID,p1,p2))
fun DECLARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(ParserData.MlyValue.VOID,p1,p2))
fun STRING (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(ParserData.MlyValue.VOID,p1,p2))
fun REAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(ParserData.MlyValue.ID (fn () => i),p1,p2))
fun CONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(ParserData.MlyValue.CONST (fn () => i),p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(ParserData.MlyValue.NUM (fn () => i),p1,p2))
end
end
