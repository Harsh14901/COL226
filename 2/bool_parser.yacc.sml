functor BoolLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Bool_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct

(* Some user decl here *)
datatype 'a BTree = Null | Node of 'a BTree * 'a * 'a BTree;


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\002\000\000\000\000\000\
\\001\000\002\000\014\000\005\000\013\000\006\000\012\000\007\000\011\000\
\\008\000\010\000\009\000\009\000\000\000\
\\001\000\003\000\008\000\004\000\007\000\010\000\006\000\013\000\005\000\
\\015\000\004\000\000\000\
\\001\000\005\000\013\000\006\000\012\000\007\000\011\000\008\000\010\000\
\\009\000\009\000\011\000\024\000\000\000\
\\001\000\005\000\013\000\006\000\012\000\007\000\011\000\008\000\010\000\
\\009\000\009\000\012\000\026\000\000\000\
\\001\000\005\000\013\000\006\000\012\000\007\000\011\000\008\000\010\000\
\\009\000\009\000\014\000\023\000\000\000\
\\029\000\000\000\
\\030\000\000\000\
\\031\000\000\000\
\\032\000\000\000\
\\033\000\000\000\
\\034\000\000\000\
\\035\000\000\000\
\\036\000\000\000\
\\037\000\005\000\013\000\006\000\012\000\007\000\011\000\008\000\010\000\
\\009\000\009\000\000\000\
\\038\000\005\000\013\000\006\000\012\000\007\000\011\000\008\000\010\000\
\\009\000\009\000\000\000\
\\039\000\000\000\
\"
val actionRowNumbers =
"\002\000\001\000\008\000\002\000\
\\002\000\002\000\007\000\002\000\
\\002\000\002\000\002\000\002\000\
\\006\000\005\000\003\000\009\000\
\\014\000\013\000\012\000\011\000\
\\010\000\016\000\002\000\004\000\
\\002\000\015\000\000\000"
val gotoT =
"\
\\001\000\026\000\002\000\001\000\000\000\
\\000\000\
\\000\000\
\\002\000\013\000\000\000\
\\002\000\014\000\000\000\
\\002\000\015\000\000\000\
\\000\000\
\\002\000\016\000\000\000\
\\002\000\017\000\000\000\
\\002\000\018\000\000\000\
\\002\000\019\000\000\000\
\\002\000\020\000\000\000\
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
\\002\000\023\000\000\000\
\\000\000\
\\002\000\025\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 27
val numrules = 11
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
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
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
datatype svalue = VOID | ntVOID of unit ->  unit
 | ID of unit ->  (string) | CONST of unit ->  (string)
 | EXP of unit ->  (string) | program of unit ->  (string)
end
type svalue = MlyValue.svalue
type result = string
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
  | (T 2) => "CONST"
  | (T 3) => "NOT"
  | (T 4) => "AND"
  | (T 5) => "OR"
  | (T 6) => "XOR"
  | (T 7) => "EQUALS"
  | (T 8) => "IMPLIES"
  | (T 9) => "IF"
  | (T 10) => "THEN"
  | (T 11) => "ELSE"
  | (T 12) => "LPAREN"
  | (T 13) => "RPAREN"
  | (T 14) => "ID"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7)
 $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( _, _, TERM1right)) :: ( _, ( MlyValue.EXP EXP1, 
EXP1left, _)) :: rest671)) => let val  result = MlyValue.program (fn _
 => let val  EXP1 = EXP1 ()
 in ("[ " ^ EXP1 ^ "]")
end)
 in ( LrTable.NT 0, ( result, EXP1left, TERM1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.CONST CONST1, CONST1left, CONST1right)) :: 
rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (CONST
 as CONST1) = CONST1 ()
 in ("CONST: "^CONST1 ^", ")
end)
 in ( LrTable.NT 1, ( result, CONST1left, CONST1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.EXP (fn _ => let val  (ID as ID1) = ID1 ()
 in ("ID: "^ID1 ^ ", ")
end)
 in ( LrTable.NT 1, ( result, ID1left, ID1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: ( _, ( _, 
NOT1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ =>
 let val  EXP1 = EXP1 ()
 in ("NOT " ^ EXP1 ^ ", ")
end)
 in ( LrTable.NT 1, ( result, NOT1left, EXP1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (EXP1 ^ " AND " ^ EXP2 ^ ", ")
end)
 in ( LrTable.NT 1, ( result, EXP1left, EXP2right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (EXP1 ^ " OR " ^ EXP2 ^ ", ")
end)
 in ( LrTable.NT 1, ( result, EXP1left, EXP2right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (EXP1 ^ " XOR " ^ EXP2 ^ ", ")
end)
 in ( LrTable.NT 1, ( result, EXP1left, EXP2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (EXP1 ^ " EQUALS " ^ EXP2 ^ ", ")
end)
 in ( LrTable.NT 1, ( result, EXP1left, EXP2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (EXP1 ^ " IMPLIES " ^ EXP2 ^ ", ")
end)
 in ( LrTable.NT 1, ( result, EXP1left, EXP2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.EXP EXP3, _, EXP3right)) :: _ :: ( _, ( 
MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: 
( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.EXP
 (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 val  EXP3 = EXP3 ()
 in ("IF " ^ EXP1 ^ " THEN " ^ EXP2 ^ " ELSE " ^ EXP3 ^ ", ")
end)
 in ( LrTable.NT 1, ( result, IF1left, EXP3right), rest671)
end
|  ( 10, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 in ("("^EXP1^"), ")
end)
 in ( LrTable.NT 1, ( result, LPAREN1left, RPAREN1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Bool_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun TERM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun CONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.CONST (fn () => i),p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun XOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun IMPLIES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
end
end
