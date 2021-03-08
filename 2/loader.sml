CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");
use "bool_parser.yacc.sig";
use "bool_parser.yacc.sml";
use "bool_expr.lex.sml";
Control.Print.printLength := 1000; (* set printing parameters so that *)
Control.Print.printDepth := 1000; (* we’ll see all details *)
Control.Print.stringDepth := 1000; (* and strings *)

exception SyntaxError;

(* Parser structure *)
structure BoolLrVals =
    BoolLrValsFun(structure Token = LrParser.Token)

(* Lexer structure *)
structure BoolLex = 
    BoolLexFun(structure Tokens = BoolLrVals.Tokens)

structure BoolParser=
      Join(structure ParserData = BoolLrVals.ParserData
          structure Lex=BoolLex
          structure LrParser=LrParser)

val dummyEOF = BoolLrVals.Tokens.EOF(0,0)

fun print_err (s, linenum, charpos) = (
  print("Syntax Error:" ^ Int.toString(linenum) ^ ":" ^ Int.toString(charpos) ^ ":" ^"<prod_rule>");
  raise SyntaxError;
  ()
);

fun invoke lexstream = BoolParser.parse(0,lexstream,print_err,());

fun stringToLexer str =
  let 
    val done = ref false
    val lexer =  BoolParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
  in
    lexer
  end	

fun parse (lexer) =
  let 
    val (result, lexer) = invoke lexer
    val (nextToken, lexer) = BoolParser.Stream.get lexer
  in
    if BoolParser.sameToken(nextToken, dummyEOF) then result
    else parse(lexer)
  end

val parseString = parse o stringToLexer
