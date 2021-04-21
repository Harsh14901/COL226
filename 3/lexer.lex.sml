(*#line 41.10 "lexer.lex"*)functor CalcLexFun(structure Tokens : Calc_TOKENS)(*#line 1.1 "lexer.lex.sml"*)
=
   struct
    structure UserDeclarations =
      struct
(*#line 1.1 "lexer.lex"*)structure Tokens= Tokens

  exception InvalidToken;

  val linep = ref 1
  val error = fn (char, linenum, pos) => (
    print("Unknown token:" ^ Int.toString(linenum) ^ ":" ^ Int.toString(pos) ^ ":" ^ char ^ "\n");
    raise InvalidToken
  );
  val eof = fn () => Tokens.EOF(!linep, !linep)
  fun refinc x =  (x := !x + 1; !x)

  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token
  val keywords =
  [
   ("div",  Tokens.DIV),
   ("end",  Tokens.END),
   ("in",  Tokens.IN),
   ("let",  Tokens.LET),
   ("val",  Tokens.VAL),
   ("if", Tokens.IF),
   ("fi", Tokens.FI),
   ("then", Tokens.THEN),
   ("else", Tokens.ELSE),
   ("NOT", Tokens.NOT),
   ("AND", Tokens.AND),
   ("OR", Tokens.OR),
   ("XOR", Tokens.XOR),
   ("IMPLIES", Tokens.IMPLIES)
   
   ]

   fun findKeywords (str:string, pos1:pos, pos2:pos) =
 ( case List.find (fn (s, _) => s = str )  keywords of 
  SOME (_, tk) => tk(pos1, pos2) 
  | NONE => Tokens.ID (str, pos1, pos2))
  (*#line 45.1 "lexer.lex.sml"*)
end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\003\003\003\003\003\003\003\003\003\027\029\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\027\003\003\003\003\003\003\003\026\025\024\023\003\022\003\003\
\\020\020\020\020\020\020\020\020\020\020\003\019\018\017\016\003\
\\003\005\005\005\005\005\011\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\007\005\005\005\005\005\005\003\003\003\003\003\
\\003\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\003\003\003\004\003\
\\003"
),
 (5, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000"
),
 (7, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\008\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000"
),
 (8, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\009\006\006\006\006\006\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000"
),
 (9, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\006\006\006\006\010\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000"
),
 (11, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\012\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000"
),
 (12, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\013\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000"
),
 (13, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\014\006\006\006\006\006\006\006\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000"
),
 (14, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\006\006\006\006\015\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000"
),
 (20, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\021\021\021\021\021\021\021\021\021\021\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (27, 
"\000\000\000\000\000\000\000\000\000\028\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\028\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = map f (rev (tl (rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [(N 43)], trans = 0},
{fin = [(N 11),(N 43)], trans = 0},
{fin = [(N 41),(N 43)], trans = 5},
{fin = [(N 41)], trans = 5},
{fin = [(N 41),(N 43)], trans = 7},
{fin = [(N 41)], trans = 8},
{fin = [(N 41)], trans = 9},
{fin = [(N 32),(N 41)], trans = 5},
{fin = [(N 41),(N 43)], trans = 11},
{fin = [(N 41)], trans = 12},
{fin = [(N 41)], trans = 13},
{fin = [(N 41)], trans = 14},
{fin = [(N 38),(N 41)], trans = 5},
{fin = [(N 21),(N 43)], trans = 0},
{fin = [(N 23),(N 43)], trans = 0},
{fin = [(N 19),(N 43)], trans = 0},
{fin = [(N 9),(N 43)], trans = 0},
{fin = [(N 7),(N 43)], trans = 20},
{fin = [(N 7)], trans = 20},
{fin = [(N 15),(N 43)], trans = 0},
{fin = [(N 13),(N 43)], trans = 0},
{fin = [(N 17),(N 43)], trans = 0},
{fin = [(N 27),(N 43)], trans = 0},
{fin = [(N 25),(N 43)], trans = 0},
{fin = [(N 4),(N 43)], trans = 27},
{fin = [(N 4)], trans = 27},
{fin = [(N 1)], trans = 0}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

structure YYPosInt : INTEGER = Int
fun makeLexer yyinput =
let	val yygone0= YYPosInt.fromInt ~1
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = substring(!yyb,i0,i-i0)
			     val yypos = YYPosInt.+(YYPosInt.fromInt i0, !yygone)
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  1 => ((*#line 48.17 "lexer.lex"*)refinc linep; lex()(*#line 268.1 "lexer.lex.sml"*)
)
| 11 => ((*#line 54.17 "lexer.lex"*)Tokens.NEGATE(!linep, yypos-1)(*#line 270.1 "lexer.lex.sml"*)
)
| 13 => ((*#line 55.17 "lexer.lex"*)Tokens.PLUS(!linep, yypos-1)(*#line 272.1 "lexer.lex.sml"*)
)
| 15 => ((*#line 56.17 "lexer.lex"*)Tokens.MINUS(!linep, yypos-1)(*#line 274.1 "lexer.lex.sml"*)
)
| 17 => ((*#line 57.17 "lexer.lex"*)Tokens.TIMES(!linep, yypos-1)(*#line 276.1 "lexer.lex.sml"*)
)
| 19 => ((*#line 58.17 "lexer.lex"*)Tokens.LESSTHAN(!linep, yypos-1)(*#line 278.1 "lexer.lex.sml"*)
)
| 21 => ((*#line 59.17 "lexer.lex"*)Tokens.GREATERTHAN(!linep, yypos-1)(*#line 280.1 "lexer.lex.sml"*)
)
| 23 => ((*#line 60.17 "lexer.lex"*)Tokens.EQUALS(!linep, yypos-1)(*#line 282.1 "lexer.lex.sml"*)
)
| 25 => ((*#line 61.17 "lexer.lex"*)Tokens.LPAREN(!linep, yypos-1)(*#line 284.1 "lexer.lex.sml"*)
)
| 27 => ((*#line 62.17 "lexer.lex"*)Tokens.RPAREN(!linep, yypos-1)(*#line 286.1 "lexer.lex.sml"*)
)
| 32 => ((*#line 63.17 "lexer.lex"*)Tokens.CONST(true ,!linep, yypos-1)(*#line 288.1 "lexer.lex.sml"*)
)
| 38 => ((*#line 64.17 "lexer.lex"*)Tokens.CONST(false ,!linep, yypos-1)(*#line 290.1 "lexer.lex.sml"*)
)
| 4 => ((*#line 49.17 "lexer.lex"*)lex()(*#line 292.1 "lexer.lex.sml"*)
)
| 41 => let val yytext=yymktext() in (*#line 65.17 "lexer.lex"*)findKeywords(yytext,!linep,yypos-1)(*#line 294.1 "lexer.lex.sml"*)
 end
| 43 => let val yytext=yymktext() in (*#line 66.17 "lexer.lex"*)error (yytext ,!linep ,yypos-1); lex()(*#line 296.1 "lexer.lex.sml"*)
 end
| 7 => let val yytext=yymktext() in (*#line 50.14 "lexer.lex"*)Tokens.NUM
	     (List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext),
	      !linep, yypos-1)(*#line 300.1 "lexer.lex.sml"*)
 end
| 9 => ((*#line 53.17 "lexer.lex"*)Tokens.TERM(!linep, yypos-1)(*#line 302.1 "lexer.lex.sml"*)
)
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof ()
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := substring(!yyb,i0,l-i0)^newchars;
		     yygone := YYPosInt.+(!yygone, YYPosInt.fromInt i0);
		     yybl := size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(CharVector.sub(!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord(CharVector.sub(trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
end
  in lex
  end
end
