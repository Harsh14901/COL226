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
";"         => (lexOut :="TERM \";\"" :: !lexOut; Tokens.TERM(!linep, yypos));
"NOT"       => (lexOut :="NOT \"NOT\"" :: !lexOut; Tokens.NOT(!linep, yypos));
"AND"       => (lexOut :="AND \"AND\"" :: !lexOut; Tokens.AND(!linep, yypos));
"OR"        => (lexOut :="OR \"OR\"" :: !lexOut; Tokens.OR(!linep, yypos));
"XOR"       => (lexOut :="XOR \"XOR\"" :: !lexOut; Tokens.XOR(!linep, yypos));
"EQUALS"    => (lexOut :="EQUALS \"EQUALS\"" :: !lexOut; Tokens.EQUALS(!linep, yypos));
"IMPLIES"   => (lexOut :="IMPLIES \"IMPLIES\"" :: !lexOut; Tokens.IMPLIES(!linep, yypos));
"IF"        => (lexOut :="IF \"IF\"" :: !lexOut; Tokens.IF(!linep, yypos));
"THEN"      => (lexOut :="THEN \"THEN\"" :: !lexOut; Tokens.THEN(!linep, yypos));
"ELSE"      => (lexOut :="ELSE \"ELSE\"" :: !lexOut; Tokens.ELSE(!linep, yypos));
"("         => (lexOut :="LPAREN \"(\"" :: !lexOut; Tokens.LPAREN(!linep, yypos));
")"         => (lexOut :="RPAREN \")\"" :: !lexOut; Tokens.RPAREN(!linep, yypos));
"TRUE"      => (lexOut :="CONST \"TRUE\"" :: !lexOut; Tokens.CONST("TRUE", !linep, yypos));
"FALSE"     => (lexOut :="CONST \"FALSE\"" :: !lexOut; Tokens.CONST("FALSE", !linep, yypos));
{alpha}+    => (lexOut :="ID \"" ^ yytext ^ "\"" :: !lexOut; Tokens.ID(yytext, !linep, yypos));
.           => (error (yytext ,!linep ,yypos); lex());