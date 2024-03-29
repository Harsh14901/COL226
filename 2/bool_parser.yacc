
(* Some user decl here *)

%%

%name Bool

%term EOF 
| TERM of string
| NOT of string 
| AND of string 
| OR of string 
| XOR of string 
| EQUALS of string 
| IMPLIES of string 
| IF of string 
| THEN of string 
| ELSE of string 
| LPAREN of string 
| RPAREN of string 
| ID of string
| CONST of string

%nonterm Statement of string 
| StatementSet of string 
| EXP of string 
| Program of string

%pos int

%eop EOF TERM
%noshift EOF

%right IF THEN ELSE
%right IMPLIES
%left AND OR XOR EQUALS
%right NOT
%nonassoc LPAREN RPAREN
%% 
Program :Statement StatementSet        (Statement1 ^ ", " ^ StatementSet1 ^ ", Program : Statement StatementSet\n")
StatementSet :                ("StatementSet :")
| Statement StatementSet      (Statement1 ^ ", " ^ StatementSet1 ^ ", Statement : Statement StatementSet") 
Statement : EXP TERM          (EXP1 ^ ", " ^ TERM1 ^ ", Statement : EXP TERM")

EXP : CONST                   (CONST1 ^ ", EXP: CONST")
  | ID                        (ID1 ^ ", EXP: ID")
  | NOT EXP                   (NOT1 ^ ", " ^ EXP1 ^ ", EXP: NOT EXP1")
  | EXP AND EXP               (EXP1 ^ ", " ^ AND1 ^ ", " ^ EXP2 ^ ", EXP: EXP1 AND EXP2")
  | EXP OR EXP                (EXP1 ^ ", " ^ OR1 ^ ", " ^ EXP2 ^ ", EXP: EXP1 OR EXP2")
  | EXP XOR EXP               (EXP1 ^ ", " ^ XOR1 ^ ", " ^ EXP2 ^ ", EXP: EXP1 XOR EXP2")
  | EXP EQUALS EXP            (EXP1 ^ ", " ^ EQUALS1 ^ ", " ^ EXP2 ^ ", EXP: EXP1 EQUALS EXP2")
  | EXP IMPLIES EXP           (EXP1 ^ ", " ^ IMPLIES1 ^ ", " ^ EXP2 ^ ", EXP: EXP1 IMPLIES EXP2")
  | IF EXP THEN EXP ELSE EXP  (IF1 ^ ", " ^ EXP1 ^ ", " ^ THEN1 ^ ", " ^ EXP2 ^ ", " ^ ELSE1 ^ ", " ^ EXP3 ^ ", EXP: IF EXP1 THEN EXP2 ELSE EXP3")
  | LPAREN EXP RPAREN         (LPAREN1 ^ ", " ^ EXP1 ^ ", " ^ RPAREN1 ^ ", EXP: LPAREN EXP1 RPAREN")
  