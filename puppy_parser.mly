%{
  open Printf

  let parse_error s =
    print_endline s
    flush stdout

  let mk_pos i =
    Parsing.rhs_start_pos i

  type A = Absyn
  (* type L = Location *)

  type S = Symbol
%}

(* 保留关键字 *)
%token EOF
%token NIL
%token VAR WHILE FOR TO BREAK LET IN END FUNCTION TYPE ARRAY IF THEN ELSE DO OF
(* 标点符号 *)
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token COMMA DOT SEMICOLON COLON ASSIGN
%token PLUS MINUS TIMES DIVIDE
%token OR AND EQUAL NOTEQUAL LT LE GT GE MINUS
%token<string> STRING

%token<string> ID
%token<int> INT

%nonassoc OF
%nonassoc DO
%nonassoc THEN
%nonassoc ELSE
%nonassoc IF
%nonassoc WHILE
%nonassoc ASSIGN
%nonassoc EQUAL NOTEQUAL LE LT GT GE
%left OR
%left AND
%left PLUS MINUS
%left TIMES DIVIDE

%type<unit> prog
%start<A.exp> prog

%%

prog:
  exp EOF { $1 }
;
decs:
  | dec { $2 }
;
dec:
  | tydec { $1 }
  | vardec { $1 }
  | fundecs { $1 }
;
tydec:
  | TYPE ID EQUAL ty { A.Tydec{name = S.symbol $1; ty = $4; pos: mk_pos 1} }
;
ty:
  | ID { A.NameTy{S.symbol $1, mk_pos 1} }
  | LBRACE tyfields RBRACE { Ast.RecordTy( $2 ) }
  | ARRAY OF ID COLON ID { Ast.ArrayTy(S.symbol $3, mk_pos 1) }
;
tyfields:
  | epsilon { [] }
  | tyfield { $1 }
  | tyfields COMMA tyfield { $2 :: $1 }
;
tyfield:
  | ID COLON ID { {name = S.symbol $1; escape = ref true; typ = S.symbol $3; pos = mk_pos 1} }
vardec:
  | VAR ID ASSIGN exp { A.VarDec{name = S.symbol $1; escape = ref true; typ = None; init = $4; pos = mk_pos 1} }
;
fundecs:
           { [] }
  | fundecs fundec { A.FunctionDec($2 :: $1) }
fundec:
  | FUNCTION ID LPAREN tyfield RPAREN EQUAL exp { A.fundec{name = Symbol $1; params = $4; result = match $4 with Some(r) -> r | None; body = $7; pos = mk_pos 1} }
;
lvalue:
  | ID { A.Ident{name = S.symbol $1; pos = mk_pos 1} }
  | lvalue DOT ID { A.RecordAccess{record = $1; name = S.symbol $3; pos = mk_pos 1} }
  | lvalue LBRACKET exp RBRACKET { A.ArrayAccess{array: $1; exp = $3; pos = mk_pos 1} }
;
exp:
  | lvalue { A.LValue{l = $1; pos = mk_pos 1} }
  | LPAREN expseq RPAREN { A.SeqExp($2) }
  | INT { A.IntExp($1) }
  | STRING { A.StringExp($1) }
  | MINUS exp { A.OpExp{left = 0.; op = MINUS; right = $2; pos = mk_pos 1} }
  | ID LBRACE records RPRACE { A.RecordExp{rec_exp_fields = $3; typ = S.symbol $1; pos = mk_pos 1} }                  (* record类型表达式 *)
  | ID LPAREN funargs RPAREN { A.CallExp{func = S.symbol $1; args = funargs; pos = mk_pos 1} }              (* 函数调用 *)
  | ID LBRAKET arryindex RBRAKET OF exp { A.ArrayExp{typ = S.symbol $1; size = $3; init = $6; pos = mk_pos 1} }
  | exp op exp { A.OpExp{left = $1; op = op; right = $3; pos = mk_pos 1} }
  | exp compar exp { A.OpExp{left = $1; op = compar; right = $3; pos = mk_pos 1} }
  | exp boolean exp { A.OpExp{left = $1; op = boolean; right = $3; pos = mk_pos 1} }
  | exp ASSIGN exp { A.AssignExp{var = $1; exp = $3; pos = mk_pos 1} }
  | IF exp THEN exp { A.IfExp{test = $2; then' = $4; else' = None; pos: mk_pos 1} }
  | IF exp THEN exp ELSE exp { A.IfExp{test = $2; then' = $4; else' = Some($6); pos = mk_pos 1} }
  | WHILE exp DO exp { A.WhileExp{test = $2; body = $4; pos = mk_pos 1} }
  | FOR ID ASSIGN exp TO exp DO exp { A.ForExp{var = S.symbol ID; escape = ref true; lo = $4; hi = $6; body = $8; pos = mk_pos 1} }
  | BREAK { A.BreakExp mk_pos 1 }
;
expseq:
  | exp { ($1, mk_pos 1) }
  | expseq SEMICOLON exp { ($3, Parsing.rhs_start_pos 3) :: expseq }
;
records:
  | record { $1 :: [] }
  | records COMMA record { $2 :: $1 }
;
record:
  | ID EQUAL exp { (S.symbol $1, $3, mk_pos 1) }
funargs:
  | epsilon { [] }
  | exp { $1 :: [] }
  | funargs COMMA exp { $3 :: $1 }
;
op:
  | PLUS { A.PlusOp }
  | MINUS { A.MinusOp }
  | TIMES { A.TimesOp }
  | DIVIDE { A.DivideOp }
;
compar:
  | LT { A.LtOp }
  | LE { A.LeOp }
  | GT { A.GtOp }
  | GE { A.GeOp }
  | EQUAL { A.EqOp }
  | NOTEQUAL { A.NeqOp }
;
boolean:
  | AND { A.AndOp }
  | OR { A.OrOp }
;

%%
