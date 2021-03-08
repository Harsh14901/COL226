
(* Some user decl here *)

%%

%name Bool

%term EOF | TERM | CONST of string | NOT | AND | OR | XOR | EQUALS | IMPLIES | IF | THEN | ELSE | LPAREN | RPAREN | ID of string
%nonterm statement | statementSet | EXP | program

%pos int

%eop EOF TERM
%noshift EOF

%right IF THEN ELSE
%right IMPLIES
%left AND OR XOR EQUALS
%right NOT
%nonassoc LPAREN RPAREN
%% 
program : statementSet        (print("program : statementSet\n"))
statementSet :                (print("statementSet :, "))
| statement statementSet      (print("statement : statement statementSet, "))               
statement : EXP TERM          (print("statement : EXP TERM, "))

EXP : CONST                   (print("CONST "^CONST1 ^", "))
  | ID                        (print("ID "^ID1 ^ ", "))
  | NOT EXP                   (print("EXP: NOT EXP1, "))
  | EXP AND EXP               (print("EXP: EXP1 AND EXP2, "))
  | EXP OR EXP                (print("EXP: EXP1 OR EXP2, "))
  | EXP XOR EXP               (print("EXP: EXP1 XOR EXP2, "))
  | EXP EQUALS EXP            (print("EXP: EXP1 EQUALS EXP2, "))
  | EXP IMPLIES EXP           (print("EXP: EXP1 IMPLIES EXP2, "))
  | IF EXP THEN EXP ELSE EXP  (print("EXP: IF EXP1 THEN EXP2 ELSE EXP3, "))
  | LPAREN EXP RPAREN         (print("EXP: LPAREN EXP1 RPAREN, "))
  |                           (print("EXP:, "))
  