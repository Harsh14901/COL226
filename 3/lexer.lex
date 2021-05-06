structure Tokens= Tokens

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
   ("IMPLIES", Tokens.IMPLIES),
   ("fn", Tokens.FN),
   ("fun", Tokens.FUN),
   ("int", Tokens.INT),
   ("bool", Tokens.BOOL),
   ("real", Tokens.REAL),
   ("string", Tokens.STRING),
   ("PLUS", Tokens.PLUS),
   ("MINUS", Tokens.MINUS),
   ("TIMES", Tokens.TIMES),
   ("NEGATE", Tokens.NEGATE),
   ("EQUALS", Tokens.EQUALS),
   ("LESSTHAN", Tokens.LESSTHAN),
   ("GREATERTHAN", Tokens.GREATERTHAN)
   
   ]

   fun findKeywords (str:string, pos1:pos, pos2:pos) =
 ( case List.find (fn (s, _) => s = str )  keywords of 
  SOME (_, tk) => tk(pos1, pos2) 
  | NONE => Tokens.ID (str, pos1, pos2))
  %% 
%header (functor CalcLexFun(structure Tokens : Calc_TOKENS));

ws = [\ \t];
alpha = [A-Za-z];
digit=[0-9];
%%
 
\n          => (refinc linep; lex());
{ws}+       => (lex());
{digit}+ => (Tokens.NUM
	     (List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext),
	      !linep, yypos-1));
";"         => (Tokens.TERM(!linep, yypos-1));
":"         => (Tokens.COLON(!linep, yypos-1));
"=>"         => (Tokens.DECLARROW(!linep, yypos-1));
"->"         => (Tokens.ARROW(!linep, yypos-1));
"="         => (Tokens.ASSIGN(!linep, yypos-1));
"("         => (Tokens.LPAREN(!linep, yypos-1));
")"         => (Tokens.RPAREN(!linep, yypos-1));
"TRUE"      => (Tokens.CONST(true ,!linep, yypos-1));
"FALSE"     => (Tokens.CONST(false ,!linep, yypos-1));
{alpha}+{digit}*    => (findKeywords(yytext,!linep,yypos-1));
.           => (error (yytext ,!linep ,yypos-1); lex());