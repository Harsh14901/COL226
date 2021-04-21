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
\\001\000\002\000\023\000\004\000\022\000\005\000\021\000\006\000\020\000\
\\007\000\019\000\008\000\018\000\015\000\017\000\016\000\016\000\
\\017\000\015\000\018\000\014\000\020\000\013\000\021\000\012\000\000\000\
\\001\000\003\000\011\000\009\000\010\000\013\000\009\000\019\000\008\000\
\\022\000\007\000\026\000\006\000\027\000\005\000\028\000\004\000\000\000\
\\001\000\004\000\022\000\005\000\021\000\006\000\020\000\007\000\019\000\
\\008\000\018\000\010\000\044\000\015\000\017\000\016\000\016\000\
\\017\000\015\000\018\000\014\000\020\000\013\000\021\000\012\000\000\000\
\\001\000\004\000\022\000\005\000\021\000\006\000\020\000\007\000\019\000\
\\008\000\018\000\011\000\050\000\015\000\017\000\016\000\016\000\
\\017\000\015\000\018\000\014\000\020\000\013\000\021\000\012\000\000\000\
\\001\000\004\000\022\000\005\000\021\000\006\000\020\000\007\000\019\000\
\\008\000\018\000\012\000\052\000\015\000\017\000\016\000\016\000\
\\017\000\015\000\018\000\014\000\020\000\013\000\021\000\012\000\000\000\
\\001\000\004\000\022\000\005\000\021\000\006\000\020\000\007\000\019\000\
\\008\000\018\000\014\000\043\000\015\000\017\000\016\000\016\000\
\\017\000\015\000\018\000\014\000\020\000\013\000\021\000\012\000\000\000\
\\001\000\004\000\022\000\005\000\021\000\006\000\020\000\007\000\019\000\
\\008\000\018\000\015\000\017\000\016\000\016\000\017\000\015\000\
\\018\000\014\000\020\000\013\000\021\000\012\000\024\000\048\000\000\000\
\\001\000\007\000\046\000\000\000\
\\001\000\023\000\041\000\000\000\
\\001\000\025\000\025\000\000\000\
\\001\000\026\000\042\000\000\000\
\\054\000\000\000\
\\055\000\004\000\022\000\005\000\021\000\006\000\020\000\007\000\019\000\
\\008\000\018\000\015\000\017\000\016\000\016\000\017\000\015\000\
\\018\000\014\000\020\000\013\000\021\000\012\000\000\000\
\\056\000\000\000\
\\057\000\000\000\
\\058\000\000\000\
\\059\000\015\000\017\000\016\000\016\000\017\000\015\000\018\000\014\000\
\\020\000\013\000\021\000\012\000\000\000\
\\060\000\000\000\
\\061\000\015\000\017\000\016\000\016\000\017\000\015\000\018\000\014\000\
\\020\000\013\000\021\000\012\000\000\000\
\\062\000\015\000\017\000\016\000\016\000\017\000\015\000\018\000\014\000\
\\020\000\013\000\021\000\012\000\000\000\
\\063\000\015\000\017\000\016\000\016\000\017\000\015\000\018\000\014\000\
\\020\000\013\000\021\000\012\000\000\000\
\\064\000\015\000\017\000\016\000\016\000\017\000\015\000\018\000\014\000\
\\020\000\013\000\021\000\012\000\000\000\
\\065\000\004\000\022\000\005\000\021\000\006\000\020\000\007\000\019\000\
\\008\000\018\000\015\000\017\000\016\000\016\000\017\000\015\000\
\\018\000\014\000\020\000\013\000\021\000\012\000\000\000\
\\066\000\017\000\015\000\018\000\014\000\000\000\
\\067\000\017\000\015\000\018\000\014\000\000\000\
\\068\000\000\000\
\\069\000\000\000\
\\070\000\015\000\017\000\016\000\016\000\017\000\015\000\018\000\014\000\000\000\
\\071\000\015\000\017\000\016\000\016\000\017\000\015\000\018\000\014\000\000\000\
\\072\000\000\000\
\\073\000\000\000\
\\074\000\000\000\
\"
val actionRowNumbers =
"\002\000\001\000\016\000\014\000\
\\015\000\010\000\002\000\002\000\
\\002\000\002\000\002\000\002\000\
\\002\000\002\000\002\000\002\000\
\\002\000\002\000\002\000\002\000\
\\002\000\012\000\009\000\011\000\
\\018\000\006\000\003\000\017\000\
\\028\000\029\000\026\000\027\000\
\\025\000\024\000\023\000\022\000\
\\021\000\020\000\019\000\002\000\
\\008\000\032\000\002\000\007\000\
\\002\000\004\000\030\000\013\000\
\\002\000\005\000\031\000\000\000"
val gotoT =
"\
\\001\000\001\000\002\000\051\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\022\000\000\000\
\\001\000\024\000\000\000\
\\001\000\025\000\000\000\
\\001\000\026\000\000\000\
\\001\000\027\000\000\000\
\\001\000\028\000\000\000\
\\001\000\029\000\000\000\
\\001\000\030\000\000\000\
\\001\000\031\000\000\000\
\\001\000\032\000\000\000\
\\001\000\033\000\000\000\
\\001\000\034\000\000\000\
\\001\000\035\000\000\000\
\\001\000\036\000\000\000\
\\001\000\037\000\000\000\
\\001\000\038\000\000\000\
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
\\000\000\
\\001\000\043\000\000\000\
\\000\000\
\\000\000\
\\001\000\045\000\000\000\
\\000\000\
\\001\000\047\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\049\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 52
val numrules = 21
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
datatype svalue = VOID | ntVOID of unit ->  unit | NUM of unit ->  (int) | CONST of unit ->  (bool) | ID of unit ->  (string) | DECL of unit ->  (AST.decl) | Program of unit ->  (AST.exp) | EXP of unit ->  (AST.exp)
end
type svalue = MlyValue.svalue
type result = AST.exp
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
  | (T 7) => "IMPLIES"
  | (T 8) => "IF"
  | (T 9) => "THEN"
  | (T 10) => "ELSE"
  | (T 11) => "FI"
  | (T 12) => "LPAREN"
  | (T 13) => "RPAREN"
  | (T 14) => "PLUS"
  | (T 15) => "MINUS"
  | (T 16) => "DIV"
  | (T 17) => "TIMES"
  | (T 18) => "NEGATE"
  | (T 19) => "LESSTHAN"
  | (T 20) => "GREATERTHAN"
  | (T 21) => "LET"
  | (T 22) => "IN"
  | (T 23) => "END"
  | (T 24) => "VAL"
  | (T 25) => "ID"
  | (T 26) => "CONST"
  | (T 27) => "NUM"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( _, _, TERM1right)) :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.Program (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in ((*#line 57.30 "parser.yacc"*)EXP(*#line 270.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 1, ( result, EXP1left, TERM1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, VAL1left, _)) :: rest671)) => let val  result = MlyValue.DECL (fn _ => let val  (ID as ID1) = ID1 ()
 val  (EXP as EXP1) = EXP1 ()
 in ((*#line 59.32 "parser.yacc"*)AST.ValDecl(ID, EXP)(*#line 276.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, VAL1left, EXP1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.CONST CONST1, CONST1left, CONST1right)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (CONST as CONST1) = CONST1 ()
 in ((*#line 61.32 "parser.yacc"*)AST.ConstExp(CONST)(*#line 283.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, CONST1left, CONST1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (ID as ID1) = ID1 ()
 in ((*#line 62.32 "parser.yacc"*)AST.VarExp(ID)(*#line 289.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, ID1left, ID1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (NUM as NUM1) = NUM1 ()
 in ((*#line 63.32 "parser.yacc"*)AST.NumExp(NUM)(*#line 295.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, NUM1left, NUM1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: ( _, ( _, NOT1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in ((*#line 64.32 "parser.yacc"*)AST.NegateExp(EXP)(*#line 301.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, NOT1left, EXP1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: ( _, ( _, NEGATE1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in ((*#line 65.32 "parser.yacc"*)AST.NegateExp(EXP)(*#line 307.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, NEGATE1left, EXP1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 66.32 "parser.yacc"*)AST.BoolExp(AST.And, EXP1, EXP2)(*#line 313.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 67.32 "parser.yacc"*)AST.BoolExp(AST.Or, EXP1, EXP2)(*#line 320.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 68.32 "parser.yacc"*)AST.BoolExp(AST.Xor, EXP1, EXP2)(*#line 327.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 69.32 "parser.yacc"*)AST.BoolExp(AST.Eq, EXP1, EXP2)(*#line 334.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 70.32 "parser.yacc"*)AST.BoolExp(AST.Implies, EXP1, EXP2)(*#line 341.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 71.32 "parser.yacc"*)AST.BinExp(AST.Add, EXP1,  EXP2)(*#line 348.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 72.32 "parser.yacc"*)AST.BinExp(AST.Sub,  EXP1,  EXP2)(*#line 355.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 73.32 "parser.yacc"*)AST.BinExp(AST.Mul,  EXP1, EXP2)(*#line 362.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 74.32 "parser.yacc"*)AST.BinExp(AST.Div, EXP1, EXP2)(*#line 369.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 75.32 "parser.yacc"*)AST.BoolExp(AST.Greater, EXP1, EXP2)(*#line 376.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 76.32 "parser.yacc"*)AST.BoolExp(AST.Less, EXP1, EXP2)(*#line 383.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 18, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.EXP EXP1, _, _)) :: _ :: ( _, ( MlyValue.DECL DECL1, _, _)) :: ( _, ( _, LET1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (DECL as DECL1) = DECL1 ()
 val  (EXP as EXP1) = EXP1 ()
 in ((*#line 77.32 "parser.yacc"*)AST.LetExp(DECL, EXP)(*#line 390.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, LET1left, END1right), rest671)
end
|  ( 19, ( ( _, ( _, _, FI1right)) :: ( _, ( MlyValue.EXP EXP3, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 val  EXP3 = EXP3 ()
 in ((*#line 78.32 "parser.yacc"*)AST.ConditionalExp(EXP1, EXP2, EXP3)(*#line 397.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, IF1left, FI1right), rest671)
end
|  ( 20, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in ((*#line 79.32 "parser.yacc"*)EXP(*#line 405.1 "parser.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
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
fun IMPLIES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(ParserData.MlyValue.VOID,p1,p2))
fun FI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(ParserData.MlyValue.VOID,p1,p2))
fun NEGATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(ParserData.MlyValue.VOID,p1,p2))
fun LESSTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(ParserData.MlyValue.VOID,p1,p2))
fun GREATERTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(ParserData.MlyValue.VOID,p1,p2))
fun VAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(ParserData.MlyValue.ID (fn () => i),p1,p2))
fun CONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(ParserData.MlyValue.CONST (fn () => i),p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(ParserData.MlyValue.NUM (fn () => i),p1,p2))
end
end
