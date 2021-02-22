exception emptyInputFile;
exception UnevenFields;
exception UnexpectedChar;

(* val escapeString = String.str(Char.chr(92));
val unPrintable = String.str(Char.chr(255)); *)

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

fun replaceAll instring findStr replaceStr = 
  let
    fun findAndReplace(i, converted) =
      let
        val sourceLength = String.size(instring)
        val keyLength = String.size(findStr);
      in
        if (i = sourceLength) then 
          converted
        else if (Int.>(i + keyLength ,sourceLength)) then
          converted ^ String.extract(instring, i, NONE)
        else if (String.substring(instring, i, keyLength) = findStr) then
          findAndReplace(i + keyLength, converted ^ replaceStr)
        else
          findAndReplace(i+1, converted ^ String.substring(instring, i , 1))
      end;
  in
    findAndReplace(0, "")
  end;


fun convertNewlines(infilename, newline1, outfilename, newline2) =
  let
    val inContent = readFile(infilename)
    val outContent = replaceAll inContent newline1 newline2
  in
    writeFile outfilename outContent
  end; 


(* 
fun convertDelimiters(infilename, delim1, outfilename, delim2) =
  let
    val inContent = readFile(infilename)
    val escapedDelim1 = escapeString ^ (String.str delim1)
    val escapedDelim2 = escapeString ^ (String.str delim2)
    
    (* convert \d to \255 *)
    val temp = replaceAll temp escapedDelim2 unPrintable
    (* convert d to \d *)
    val temp = replaceAll temp (String.str delim2) escapedDelim2
    (* convert \255 to \\d *)
    val temp = replaceAll temp unPrintable (escapeString ^ escapedDelim2)

    (* convert \c to \255 *)
    val temp = replaceAll temp escapedDelim1 unPrintable 
    (* convert c to d *)
    val temp = replaceAll temp (String.str delim1) (String.str delim2)
    (* convert \255 to c *)
    val temp = replaceAll temp unPrintable (String.str delim1)
    val outContent = temp
  in
    writeFile outfilename outContent
  end;  *)



(* fun validateInput inContent delim newline =
  let

    val firstLine::lines = String.tokens (fn x => x = newline) inContent
    fun getNumFields line = length(String.fields (fn x => x = delim) line)
    val numBaseFields = getNumFields firstLine
    fun checkFields([], i) = true
      | checkFields(x::xs, i) = 
        let
          val numFields = getNumFields x
        in
          if numFields <> numBaseFields then 
            raise UnevenFields("Expected: " ^ Int.toString(numBaseFields) ^ " fields, Present: " ^ Int.toString(numFields) ^ " fields on Line: " ^ Int.toString(i) ^ "\n")
          else 
            checkFields(xs, i + 1)
        end;
  in
    checkFields(lines, 2)line. ... Newline inside a value seems to work if you use semicolon as separator, instead of comma
    handle UnevenFields msg => not (print msg = ())
                      | exn => false
  end
   *)


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


fun unix2dos(infilename, outfilename) = convertNewlines(infilename, "\n", outfilename, "\r\n")
fun dos2unix(infilename, outfilename) = convertNewlines(infilename, "\r\n", outfilename, "\n")