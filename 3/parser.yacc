
(* Some user decl here *)

%%

%name Calc

%term EOF 
| TERM
| NOT 
| AND 
| OR 
| XOR 
| EQUALS 
| IMPLIES 
| IF 
| THEN 
| ELSE 
| FI
| LPAREN 
| RPAREN 
| PLUS
| MINUS
| DIV
| TIMES
| NEGATE
| LESSTHAN
| GREATERTHAN
| LET
| IN 
| END
| VAL
| ID of string
| CONST of bool
| NUM of int

%nonterm EXP of AST.exp 
| Program of AST.exp
| DECL of AST.decl

%pos int

%eop EOF TERM
%noshift EOF

%right IF THEN ELSE FI
%right LET IN END
%right IMPLIES
%left AND OR XOR EQUALS
%right NOT
%left LESSTHAN GREATERTHAN
%left PLUS MINUS
%left TIMES DIV
%nonassoc NEGATE
%nonassoc LPAREN RPAREN
%% 
Program : EXP TERM          (EXP)

DECL: VAL ID EQUALS EXP       (AST.ValDecl(ID, EXP))

EXP : CONST                   (AST.ConstExp(CONST))
  | ID                        (AST.VarExp(ID))
  | NUM                       (AST.NumExp(NUM))
  | NOT EXP                   (AST.NegateExp(EXP))
  | NEGATE EXP                (AST.NegateExp(EXP))
  | EXP AND EXP               (AST.BoolExp(AST.And, EXP1, EXP2))
  | EXP OR EXP                (AST.BoolExp(AST.Or, EXP1, EXP2))
  | EXP XOR EXP               (AST.BoolExp(AST.Xor, EXP1, EXP2))
  | EXP EQUALS EXP            (AST.BoolExp(AST.Eq, EXP1, EXP2))
  | EXP IMPLIES EXP           (AST.BoolExp(AST.Implies, EXP1, EXP2))
  | EXP PLUS EXP              (AST.BinExp(AST.Add, EXP1,  EXP2))
  | EXP MINUS EXP             (AST.BinExp(AST.Sub,  EXP1,  EXP2))
  | EXP TIMES  EXP            (AST.BinExp(AST.Mul,  EXP1, EXP2))
  | EXP DIV  EXP              (AST.BinExp(AST.Div, EXP1, EXP2))
  | EXP GREATERTHAN  EXP      (AST.BoolExp(AST.Greater, EXP1, EXP2))
  | EXP LESSTHAN  EXP         (AST.BoolExp(AST.Less, EXP1, EXP2))
  | LET DECL IN EXP END       (AST.LetExp(DECL, EXP))
|IF EXP THEN EXP ELSE EXP FI  (AST.ConditionalExp(EXP1, EXP2, EXP3))
  | LPAREN EXP RPAREN         (EXP)
  