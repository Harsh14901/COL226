exception emptyInputFile;
exception UnevenFields;
exception UnexpectedChar;

fun readFile filename =
    let
      val instream = TextIO.openIn filename
      val content = TextIO.inputAll instream handle e => (TextIO.closeIn instream; raise e)
      val _ = TextIO.closeIn instream
    in 
      if String.size(content) = 0 then 
        raise emptyInputFile
      else
        content
    end;

fun writeFile filename content =
    let
      val outstream = TextIO.openOut filename
      val _ = TextIO.output (outstream, content) handle e => (TextIO.closeOut outstream; raise e)
      val _ = TextIO.closeOut outstream
    in
      ()
    end;

fun convertDelimiters(infilename, delim1, outfilename, delim2) = 
let
  datatype states = start | singleQuote | unquotedField | doubleQuote | eol 
  fun contentParser([], fieldCount, totalFields, state, lineNum, outList) =
      if(state <> start) then 
        (print("File stream terminated unexpectedly at line: " ^ Int.toString(lineNum) ^ "\n"); raise UnexpectedChar; [])
      else
        outList
      
    | contentParser(c::cl, fieldCount, totalFields, state, lineNum, outList) =
      case state of
        start => 
          if (c = #"\"") then 
            contentParser(cl, fieldCount, totalFields, singleQuote, lineNum, c::outList)
          else if (c = #"\n") then
            (print("Found a record terminating with a delimiter or a record beginning with new line at line: " ^ Int.toString(lineNum) ^ "\n"); raise UnexpectedChar; [])
          else if (c = delim1) then
            contentParser(cl, fieldCount + 1, totalFields, start, lineNum, delim2::outList)
          else 
            contentParser(cl, fieldCount, totalFields, unquotedField, lineNum, c::(#"\""::outList))

      | singleQuote =>
          if (c = #"\"") then
            contentParser(cl, fieldCount, totalFields, doubleQuote, lineNum, c::outList)
          else  
            contentParser(cl, fieldCount, totalFields, singleQuote, lineNum, c::outList)
    
      | doubleQuote =>
        if (c = #"\"") then
          contentParser(cl, fieldCount, totalFields, singleQuote, lineNum, c::outList)
        else if (c = delim1) then
          contentParser(cl, fieldCount + 1, totalFields, start, lineNum, delim2::outList)
        else if (c = #"\n") then
          contentParser(c::cl, fieldCount + 1, totalFields, eol, lineNum, outList)
        else
          (print("Found an unexpected character " ^ String.str(c) ^ " at line: " ^ Int.toString(lineNum) ^ "\n"); raise UnexpectedChar; [])
      | unquotedField =>
        if (c = #"\"") then
          (print("Found a misplaced quote at line number: " ^ Int.toString(lineNum) ^ "\n"); raise UnexpectedChar; [])
        else if (c = delim1) then
          contentParser(cl, fieldCount + 1, totalFields, start, lineNum, delim2::(#"\""::outList))
        else if (c = #"\n") then
          contentParser(c::cl, fieldCount + 1, totalFields, eol, lineNum, (#"\""::outList))
        else
          contentParser(cl, fieldCount, totalFields, unquotedField, lineNum, c::outList)

      | eol =>
        if (totalFields = ~1) then
          contentParser(cl, 0, fieldCount, start, lineNum + 1, c::outList)
        else if (totalFields <> fieldCount) then
          (print("Expected: " ^ Int.toString(totalFields) ^ " fields, Present: " ^ Int.toString(fieldCount) ^ " fields on Line: " ^ Int.toString(lineNum) ^ "\n"); raise UnevenFields; [])
        else 
          contentParser(cl, 0, totalFields, start, lineNum + 1, c::outList)

  val inList = String.explode(readFile(infilename))
  
  val outContent = String.implode(List.rev(contentParser(inList, 0, ~1, start, 1, [])))
in
  writeFile outfilename outContent
  
end

fun csv2tsv(infilename, outfilename) = convertDelimiters(infilename, #",", outfilename, #"\t")
fun tsv2csv(infilename, outfilename) = convertDelimiters(infilename, #"\t", outfilename, #",")
