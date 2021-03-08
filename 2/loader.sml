exception SyntaxError;
exception EmptyInputFile
exception InvalidUsage

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
  print("Syntax Error:" ^ Int.toString(linenum) ^ ":" ^ Int.toString(charpos) ^ ":" ^s^"\n");
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

fun readFile filename =
  let
    val instream = TextIO.openIn filename
    val content = TextIO.inputAll instream handle e => (TextIO.closeIn instream; raise e)
    val _ = TextIO.closeIn instream
  in 
    if String.size(content) = 0 then 
      raise EmptyInputFile
    else
      content
  end;


fun main() = 
  let
    val args = CommandLine.arguments();
  in
    if(length(args) = 0) then
      (print("Usage: ./a2 <filename>\n"); raise InvalidUsage; ())
    else
      let
        val filename = hd(args)
        val content = readFile filename
        val output = parseString(content)
      in
        print(output)
      end
  end

val _ = main()