exception InvalidToken;
val lexOut : string list ref = ref []
val linep = ref 1
val error = fn (char, linenum, pos) => (
  print("Unknown token:" ^ Int.toString(linenum) ^ ":" ^ Int.toString(pos) ^ ":" ^ char ^ "\n");
  raise InvalidToken
);
val eof = fn () => 
  let
    val accumulate = fn(next, res) => res ^ ", " ^ next
    val revOut = List.rev (!lexOut)
    val output = List.foldl accumulate ("[" ^ hd(revOut)) (tl(revOut))

    val _ = (lexOut := []; print(output ^ "]\n"))
  in
    Tokens.EOF(!linep, !linep)
  end
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
";"         => (lexOut :="TERM \"" ^ yytext ^ "\"" :: !lexOut; Tokens.TERM(hd(!lexOut), !linep, yypos));
"NOT"       => (lexOut :="NOT \"" ^ yytext ^ "\"" :: !lexOut; Tokens.NOT(hd(!lexOut), !linep, yypos));
"AND"       => (lexOut :="AND \"" ^ yytext ^ "\"" :: !lexOut; Tokens.AND(hd(!lexOut), !linep, yypos));
"OR"        => (lexOut :="OR \"" ^ yytext ^ "\"" :: !lexOut; Tokens.OR(hd(!lexOut), !linep, yypos));
"XOR"       => (lexOut :="XOR \"" ^ yytext ^ "\"" :: !lexOut; Tokens.XOR(hd(!lexOut), !linep, yypos));
"EQUALS"    => (lexOut :="EQUALS \"" ^ yytext ^ "\"" :: !lexOut; Tokens.EQUALS(hd(!lexOut), !linep, yypos));
"IMPLIES"   => (lexOut :="IMPLIES \"" ^ yytext ^ "\"" :: !lexOut; Tokens.IMPLIES(hd(!lexOut), !linep, yypos));
"IF"        => (lexOut :="IF \"" ^ yytext ^ "\"" :: !lexOut; Tokens.IF(hd(!lexOut), !linep, yypos));
"THEN"      => (lexOut :="THEN \"" ^ yytext ^ "\"" :: !lexOut; Tokens.THEN(hd(!lexOut), !linep, yypos));
"ELSE"      => (lexOut :="ELSE \"" ^ yytext ^ "\"" :: !lexOut; Tokens.ELSE(hd(!lexOut), !linep, yypos));
"("         => (lexOut :="LPAREN \"" ^ yytext ^ "\"" :: !lexOut; Tokens.LPAREN(hd(!lexOut), !linep, yypos));
")"         => (lexOut :="RPAREN \"" ^ yytext ^ "\"" :: !lexOut; Tokens.RPAREN(hd(!lexOut), !linep, yypos));
"TRUE"      => (lexOut :="CONST \"" ^ yytext ^ "\"" :: !lexOut; Tokens.CONST(hd(!lexOut), !linep, yypos));
"FALSE"     => (lexOut :="CONST \"" ^ yytext ^ "\"" :: !lexOut; Tokens.CONST(hd(!lexOut), !linep, yypos));
{alpha}+    => (lexOut :="ID \"" ^ yytext ^ "\"" :: !lexOut; Tokens.ID(hd(!lexOut), !linep, yypos));
.           => (error (yytext ,!linep ,yypos); lex());