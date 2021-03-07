
(* Some user decl here *)
datatype 'a BTree = Null | Node of 'a BTree * 'a * 'a BTree;

%%

%name Bool

%term EOF | TERM | CONST of string | NOT | AND | OR | XOR | EQUALS | IMPLIES | IF | THEN | ELSE | LPAREN | RPAREN | ID of string
%nonterm program of string | EXP of string

%pos int

%eop EOF TERM
%noshift EOF

%right IF THEN ELSE
%right IMPLIES
%left AND OR XOR EQUALS
%right NOT
%nonassoc LPAREN RPAREN
%% 

program : EXP TERM ("[ " ^ EXP1 ^ "]")

EXP : CONST  ("CONST: "^CONST1 ^", ")
  | ID ("ID: "^ID1 ^ ", ")
  | NOT EXP ("NOT " ^ EXP1 ^ ", ")
  | EXP AND EXP (EXP1 ^ " AND " ^ EXP2 ^ ", ")
  | EXP OR EXP (EXP1 ^ " OR " ^ EXP2 ^ ", ")
  | EXP XOR EXP (EXP1 ^ " XOR " ^ EXP2 ^ ", ")
  | EXP EQUALS EXP (EXP1 ^ " EQUALS " ^ EXP2 ^ ", ")
  | EXP IMPLIES EXP (EXP1 ^ " IMPLIES " ^ EXP2 ^ ", ")
  | IF EXP THEN EXP ELSE EXP ("IF " ^ EXP1 ^ " THEN " ^ EXP2 ^ " ELSE " ^ EXP3 ^ ", ")
  | LPAREN EXP RPAREN ("("^EXP1^"), ")
  


