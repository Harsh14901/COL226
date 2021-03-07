exception InvalidToken;
(* datatype lexresult =  EOF | TERM | CONST of string | NOT | AND | OR | XOR | EQUALS | IMPLIES | IF | THEN | ELSE | LPAREN | RPAREN | ID of string *)
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
%% 
%header (functor BoolLexFun(structure Tokens : Bool_TOKENS));

ws = [\ \t];
alpha = [A-Za-z];

%%
 
\n          => (refinc linep; lex());
{ws}+       => (lex());
";"         => (Tokens.TERM(!linep, yypos));
"NOT"       => (Tokens.NOT(!linep, yypos));
"AND"       => (Tokens.AND(!linep, yypos));
"OR"        => (Tokens.OR(!linep, yypos));
"XOR"       => (Tokens.XOR(!linep, yypos));
"EQUALS"    => (Tokens.EQUALS(!linep, yypos));
"IMPLIES"   => (Tokens.IMPLIES(!linep, yypos));
"IF"        => (Tokens.IF(!linep, yypos));
"THEN"      => (Tokens.THEN(!linep, yypos));
"ELSE"      => (Tokens.ELSE(!linep, yypos));
"("         => (Tokens.LPAREN(!linep, yypos));
")"         => (Tokens.RPAREN(!linep, yypos));
"TRUE"      => (Tokens.CONST("TRUE", !linep, yypos));
"FALSE"     => (Tokens.CONST("FALSE", !linep, yypos));
{alpha}+    => (Tokens.ID(yytext, !linep, yypos));
.           => (error (yytext ,!linep ,yypos); lex());