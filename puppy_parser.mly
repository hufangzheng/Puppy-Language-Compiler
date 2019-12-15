%{
  open Printf

  let parse_error s =
    print_endline s
    flush stdout

  type A = Absyn
  type L = Location
%}

%token EOF
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token COMMA DOT SEMICOLON COLON ASSIGN
%token VAR WHILE FOR TO BREAK LET IN END FUNCTION TYPE ARRAY IF THEN ELSE DO OF
%token PLUS MINUS TIMES DIVIDE
%token OR AND EQUAL NOTEQUAL LT LE GT GE MINUS
%token NIL
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
%start prog

%%

prog:
  expr EOF {}
;
decs:
  | dec {}
;
dec:
  | tydec {A.TypeDec()}
  | vardec {}
  | fundec {}
;
tydec:
  | TYPE ID EQUAL ty {}
;
ty:
  | ID {}
  | LBRACE tyfield RBRACE {}
  | ARRAY OF ID COLON ID {}
;
tyfield:
  | epsilon {}
  | ID COLON ID {}
  | ID COLON ID COMMA tyfield {}
;
vardec:
  | VAR ID ASSIGN exp {}
;
fundec:
  | FUNCTION ID LPAREN tyfield RPAREN EQUAL exp {}
;
lvalue:
  | ID {}
  | lvalue DOT ID {}
  | lvalue LBRACKET exp RBRACKET {}
;
exp:
  | lvalue {}
  | LPAREN expseq RPAREN {}
  | INT {}
  | STRING {}
  | MINUS exp {}
  | ID LBRACE records RPRACE {}
  | ID LPAREN funargs RPAREN {}
  | ID LBRAKET arryindex RBRAKET OF exp {}
  | exp op exp {}
  | exp compar exp {}
  | exp boolean exp {}
  | exp ASSIGN exp {}
  | IF exp THEN exp {}
  | IF exp THEN exp ELSE exp {}
  | WHILE exp DO exp {}
  | FOR ID ASSIGN exp TO exp DO exp {}
  | BREAK
;
expseq:
  | exp {}
  | exp SEMICOLON exp {}
;
records:
  | epsilon {}
  | exp {}
  | exp COMMA exp {}
;
funargs:
  | epsilon {}
  | exp {}
  | exp COMMA exp {}
;
arrayindex:
  | epsilon {}
  | exp {}
op:
  | PLUS {}
  | MINUS {}
  | TIMES {}
  | DIVIDE {}
;
compar:
  | LT {}
  | LE {}
  | GT {}
  | GE {}
  | EQUAL {}
  | NOTEQUAL {}
;
boolean:
  | AND {}
  | OR {}
;

%%
