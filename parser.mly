%{
  open Printf

  let parse_error s =
    print_endline s;
    flush stdout

  let mk_pos i =
    Parsing.rhs_start_pos i
%}

/* Keyword */
%token EOF
%token NIL
%token VAR WHILE FOR TO BREAK LET IN END FUNCTION TYPE ARRAY IF THEN ELSE DO OF
/* Punctuation */
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

%start prog
%type <Absyn.exp> prog

%%

prog:
  exp EOF { $1 }
;
decs:
  |     { [] }
  | dec { $1 :: [] }
  | decs dec { $2 :: $1 }
;
dec:
  | tydec { $1 }
  | vardec { $1 }
  | fundecs { Absyn.FunctionDec($1) }
;
tydec:
  | TYPE ID EQUAL ty { Absyn.TypeDec{name = Symbol.symbol $2; ty = $4; pos = mk_pos 1} }
;
ty:
  | ID { Absyn.NameTy(Symbol.symbol $1, mk_pos 1) }
  | LBRACE tyfields RBRACE { Absyn.RecordTy( $2 ) }
  | ARRAY OF ID COLON ID { Absyn.ArrayTy(Symbol.symbol $3, mk_pos 1) }
;
tyfields:
  |         { [] }
  | tyfield { $1 :: [] }
  | tyfields COMMA tyfield { $3 :: $1 }
;
tyfield:
  | ID COLON ID { {name = Symbol.symbol $1; escape = ref true; typ = Symbol.symbol $3; pos = mk_pos 1} }
vardec:
  | VAR ID ASSIGN exp { Absyn.VarDec{name = Symbol.symbol $2; escape = ref true; typ = None; init = $4; pos = mk_pos 1} }
;
fundecs:
  | fundec         { $1 :: [] }
  | fundecs fundec { $2 :: $1 }
fundec:
  | FUNCTION ID LPAREN tyfields RPAREN resultty EQUAL exp { {name = Symbol.symbol $2; params = $4; result = $6; body = $8; pos = mk_pos 1} }
;
resultty:
  |       { None }
  | COLON ID  { Some(Symbol.symbol $2, mk_pos 2) }
lvalue:
  | ID { Absyn.Ident{name = Symbol.symbol $1; pos = mk_pos 1} }
  | lvalue DOT ID { Absyn.RecordAccess{record = $1; name = Symbol.symbol $3; pos = mk_pos 1} }
  | lvalue LBRACKET exp RBRACKET { Absyn.ArrayAccess{array = $1; exp = $3; pos = mk_pos 1} }
;
exp:
  | NIL { Absyn.NilExp(mk_pos 1) }
  | lvalue { Absyn.LValue{l = $1; pos = mk_pos 1} }
  | LPAREN expseq RPAREN { Absyn.SeqExp($2) }
  | INT { Absyn.IntExp($1) }
  | STRING { Absyn.StringExp($1, mk_pos 1) }
  | MINUS exp { Absyn.OpExp{left = Absyn.IntExp(0); op = Absyn.MinusOp; right = $2; pos = mk_pos 1} }
/* record类型表达式 */
  | ID LBRACE records RBRACE { Absyn.RecordExp{rec_exp_fields = $3; typ = Symbol.symbol $1; pos = mk_pos 1} }
/* 函数调用 */
  | ID LPAREN funargs RPAREN { Absyn.CallExp{func = Symbol.symbol $1; args = $3; pos = mk_pos 1} }
  | ID LBRACKET exp RBRACKET OF exp { Absyn.ArrayExp{typ = Symbol.symbol $1; size = $3; init = $6; pos = mk_pos 1} }
  | exp op exp { Absyn.OpExp{left = $1; op = $2; right = $3; pos = mk_pos 1} }
  | exp compar exp { Absyn.OpExp{left = $1; op = $2; right = $3; pos = mk_pos 1} }
  | exp boolean exp { Absyn.OpExp{left = $1; op = $2; right = $3; pos = mk_pos 1} }
  | LET decs IN exp END { Absyn.LetExp{decs = $2; body = $4; pos = mk_pos 1} }
  | lvalue ASSIGN exp { Absyn.AssignExp{lvalue = $1; exp = $3; pos = mk_pos 1} }
  | IF exp THEN exp { Absyn.IfExp{test = $2; then' = $4; else' = None; pos = mk_pos 1} }
  | IF exp THEN exp ELSE exp { Absyn.IfExp{test = $2; then' = $4; else' = Some($6); pos = mk_pos 1} }
  | WHILE exp DO exp { Absyn.WhileExp{test = $2; body = $4; pos = mk_pos 1} }
  | FOR ID ASSIGN exp TO exp DO exp { Absyn.ForExp{var = Symbol.symbol $2; escape = ref true; lo = $4; hi = $6; body = $8; pos = mk_pos 1} }
  | BREAK { Absyn.BreakExp (mk_pos 1) }
;
expseq:
  | exp { ($1, mk_pos 1) :: [] }
  | expseq SEMICOLON exp { ($3, Parsing.rhs_start_pos 3) :: $1 }
;
records:
  | record { $1 :: [] }
  | records COMMA record { $3 :: $1 }
;
record:
  | ID EQUAL exp { (Symbol.symbol $1, $3, mk_pos 1) }
funargs:
  |     { [] }
  | exp { $1 :: [] }
  | funargs COMMA exp { $3 :: $1 }
;
op:
  | PLUS { Absyn.PlusOp }
  | MINUS { Absyn.MinusOp }
  | TIMES { Absyn.TimesOp }
  | DIVIDE { Absyn.DivideOp }
;
compar:
  | LT { Absyn.LtOp }
  | LE { Absyn.LeOp }
  | GT { Absyn.GtOp }
  | GE { Absyn.GeOp }
  | EQUAL { Absyn.EqOp }
  | NOTEQUAL { Absyn.NeqOp }
;
boolean:
  | AND { Absyn.AndOp }
  | OR { Absyn.OrOp }
;

%%
