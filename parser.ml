type token =
  | EOF
  | NIL
  | VAR
  | WHILE
  | FOR
  | TO
  | BREAK
  | LET
  | IN
  | END
  | FUNCTION
  | TYPE
  | ARRAY
  | IF
  | THEN
  | ELSE
  | DO
  | OF
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | COMMA
  | DOT
  | SEMICOLON
  | COLON
  | ASSIGN
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | OR
  | AND
  | EQUAL
  | NOTEQUAL
  | LT
  | LE
  | GT
  | GE
  | STRING of (string)
  | ID of (string)
  | INT of (int)

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Printf

  let parse_error s =
    print_endline s;
    flush stdout

  let mk_pos i =
    Parsing.rhs_start_pos i
# 59 "parser.ml"
let yytransl_const = [|
    0 (* EOF *);
  257 (* NIL *);
  258 (* VAR *);
  259 (* WHILE *);
  260 (* FOR *);
  261 (* TO *);
  262 (* BREAK *);
  263 (* LET *);
  264 (* IN *);
  265 (* END *);
  266 (* FUNCTION *);
  267 (* TYPE *);
  268 (* ARRAY *);
  269 (* IF *);
  270 (* THEN *);
  271 (* ELSE *);
  272 (* DO *);
  273 (* OF *);
  274 (* LPAREN *);
  275 (* RPAREN *);
  276 (* LBRACE *);
  277 (* RBRACE *);
  278 (* LBRACKET *);
  279 (* RBRACKET *);
  280 (* COMMA *);
  281 (* DOT *);
  282 (* SEMICOLON *);
  283 (* COLON *);
  284 (* ASSIGN *);
  285 (* PLUS *);
  286 (* MINUS *);
  287 (* TIMES *);
  288 (* DIVIDE *);
  289 (* OR *);
  290 (* AND *);
  291 (* EQUAL *);
  292 (* NOTEQUAL *);
  293 (* LT *);
  294 (* LE *);
  295 (* GT *);
  296 (* GE *);
    0|]

let yytransl_block = [|
  297 (* STRING *);
  298 (* ID *);
  299 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\003\000\003\000\004\000\004\000\004\000\005\000\
\008\000\008\000\008\000\009\000\009\000\009\000\010\000\006\000\
\007\000\007\000\011\000\012\000\012\000\013\000\013\000\013\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\014\000\014\000\015\000\015\000\020\000\
\016\000\016\000\016\000\017\000\017\000\017\000\017\000\018\000\
\018\000\018\000\018\000\018\000\018\000\019\000\019\000\000\000"

let yylen = "\002\000\
\002\000\000\000\001\000\002\000\001\000\001\000\001\000\004\000\
\001\000\003\000\005\000\000\000\001\000\003\000\003\000\004\000\
\001\000\002\000\008\000\000\000\002\000\001\000\003\000\004\000\
\001\000\001\000\003\000\001\000\001\000\002\000\004\000\004\000\
\006\000\003\000\003\000\003\000\005\000\003\000\004\000\006\000\
\004\000\008\000\001\000\001\000\003\000\001\000\003\000\003\000\
\000\000\001\000\003\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\025\000\000\000\000\000\043\000\000\000\000\000\
\000\000\000\000\029\000\000\000\028\000\064\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\003\000\005\000\
\006\000\000\000\017\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\001\000\052\000\053\000\054\000\055\000\063\000\
\062\000\060\000\061\000\056\000\057\000\058\000\059\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\004\000\018\000\000\000\027\000\000\000\
\000\000\000\000\000\000\000\000\046\000\000\000\000\000\000\000\
\000\000\000\000\023\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\032\000\000\000\000\000\031\000\
\000\000\000\000\024\000\000\000\000\000\000\000\000\000\013\000\
\000\000\000\000\009\000\008\000\037\000\000\000\000\000\000\000\
\047\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\015\000\000\000\000\000\014\000\000\000\
\010\000\000\000\021\000\000\000\000\000\000\000\011\000"

let yydgoto = "\002\000\
\014\000\015\000\022\000\023\000\024\000\025\000\026\000\100\000\
\095\000\096\000\027\000\118\000\016\000\030\000\068\000\066\000\
\048\000\049\000\050\000\069\000"

let yysindex = "\008\000\
\011\255\000\000\000\000\011\255\237\254\000\000\009\255\011\255\
\011\255\011\255\000\000\087\255\000\000\000\000\091\000\068\255\
\167\255\020\255\015\255\043\255\058\255\023\255\000\000\000\000\
\000\000\096\255\000\000\142\255\072\001\016\255\007\255\011\255\
\066\255\011\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\011\255\
\011\255\011\255\011\255\072\255\011\255\011\255\011\255\071\255\
\098\255\082\255\011\255\000\000\000\000\011\255\000\000\011\255\
\072\001\240\254\084\255\035\255\000\000\042\001\072\001\072\001\
\072\001\060\001\000\000\072\001\072\001\032\255\011\255\079\255\
\246\254\103\255\129\255\072\001\000\000\011\255\011\255\000\000\
\066\255\105\255\000\000\011\255\072\001\097\255\002\255\000\000\
\106\255\079\255\000\000\000\000\000\000\011\255\072\001\072\001\
\000\000\011\255\202\255\083\255\099\255\079\255\085\255\053\255\
\072\001\072\001\011\255\000\000\086\255\110\255\000\000\102\255\
\000\000\072\001\000\000\011\255\104\255\072\001\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\123\255\000\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\040\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\005\255\000\000\000\000\017\255\000\000\079\000\003\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\057\255\000\000\000\000\000\000\000\000\000\000\130\000\150\000\
\170\000\000\000\000\000\190\000\210\000\000\000\000\000\070\255\
\000\000\000\000\043\001\054\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\076\255\000\000\000\000\000\000\
\000\000\089\255\000\000\000\000\000\000\000\000\073\255\094\255\
\000\000\000\000\000\000\000\000\112\255\000\000\000\000\000\000\
\230\000\250\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\014\001\000\000\000\000\000\000\093\255\000\000"

let yygindex = "\000\000\
\000\000\252\255\000\000\126\000\000\000\000\000\000\000\000\000\
\051\000\041\000\127\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\063\000"

let yytablesize = 624
let yytable = "\017\000\
\022\000\097\000\085\000\028\000\029\000\031\000\007\000\086\000\
\001\000\098\000\019\000\003\000\007\000\004\000\005\000\007\000\
\006\000\007\000\020\000\021\000\109\000\049\000\018\000\008\000\
\019\000\110\000\049\000\065\000\009\000\070\000\059\000\099\000\
\020\000\021\000\063\000\044\000\092\000\038\000\039\000\026\000\
\010\000\064\000\044\000\071\000\072\000\073\000\074\000\055\000\
\076\000\077\000\078\000\011\000\012\000\013\000\082\000\088\000\
\056\000\083\000\089\000\084\000\036\000\037\000\038\000\039\000\
\040\000\041\000\042\000\043\000\044\000\045\000\046\000\047\000\
\045\000\121\000\093\000\050\000\110\000\016\000\030\000\045\000\
\050\000\103\000\104\000\016\000\057\000\016\000\016\000\107\000\
\012\000\051\000\035\000\051\000\052\000\012\000\019\000\053\000\
\051\000\113\000\079\000\058\000\019\000\114\000\019\000\019\000\
\032\000\020\000\033\000\067\000\034\000\012\000\122\000\101\000\
\012\000\075\000\048\000\080\000\081\000\048\000\087\000\126\000\
\094\000\106\000\111\000\108\000\116\000\117\000\120\000\123\000\
\125\000\034\000\002\000\036\000\037\000\038\000\039\000\040\000\
\041\000\042\000\043\000\044\000\045\000\046\000\047\000\102\000\
\124\000\127\000\020\000\060\000\112\000\035\000\119\000\105\000\
\061\000\000\000\000\000\062\000\000\000\036\000\037\000\038\000\
\039\000\040\000\041\000\042\000\043\000\044\000\045\000\046\000\
\047\000\036\000\036\000\037\000\038\000\039\000\040\000\041\000\
\042\000\043\000\044\000\045\000\046\000\047\000\054\000\000\000\
\000\000\000\000\000\000\000\000\000\000\038\000\000\000\000\000\
\000\000\000\000\000\000\036\000\037\000\038\000\039\000\040\000\
\041\000\042\000\043\000\044\000\045\000\046\000\047\000\000\000\
\000\000\041\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\115\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\040\000\036\000\037\000\
\038\000\039\000\040\000\041\000\042\000\043\000\044\000\045\000\
\046\000\047\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\033\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\022\000\000\000\000\000\022\000\000\000\000\000\
\022\000\022\000\022\000\022\000\000\000\042\000\022\000\022\000\
\022\000\000\000\000\000\022\000\000\000\022\000\000\000\022\000\
\022\000\022\000\022\000\000\000\022\000\022\000\022\000\022\000\
\022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
\022\000\026\000\039\000\000\000\026\000\000\000\000\000\026\000\
\026\000\026\000\026\000\000\000\000\000\026\000\026\000\026\000\
\000\000\000\000\026\000\000\000\026\000\000\000\026\000\026\000\
\000\000\026\000\000\000\000\000\026\000\026\000\026\000\026\000\
\026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
\030\000\000\000\000\000\030\000\000\000\000\000\030\000\030\000\
\030\000\030\000\000\000\000\000\030\000\030\000\030\000\000\000\
\000\000\030\000\000\000\030\000\000\000\030\000\030\000\000\000\
\030\000\000\000\000\000\030\000\030\000\000\000\000\000\030\000\
\030\000\030\000\030\000\030\000\030\000\030\000\030\000\036\000\
\037\000\038\000\039\000\040\000\041\000\042\000\043\000\044\000\
\045\000\046\000\047\000\034\000\000\000\000\000\034\000\000\000\
\000\000\034\000\034\000\034\000\034\000\000\000\000\000\034\000\
\034\000\034\000\000\000\000\000\034\000\000\000\034\000\035\000\
\034\000\034\000\035\000\034\000\000\000\035\000\035\000\035\000\
\035\000\000\000\000\000\035\000\035\000\035\000\000\000\000\000\
\035\000\000\000\035\000\036\000\035\000\035\000\036\000\035\000\
\000\000\036\000\036\000\036\000\036\000\000\000\000\000\036\000\
\036\000\036\000\000\000\000\000\036\000\000\000\036\000\038\000\
\036\000\036\000\038\000\036\000\000\000\038\000\038\000\038\000\
\038\000\000\000\000\000\038\000\038\000\038\000\000\000\000\000\
\038\000\000\000\038\000\041\000\038\000\038\000\041\000\038\000\
\000\000\041\000\041\000\041\000\041\000\000\000\000\000\041\000\
\041\000\041\000\000\000\000\000\041\000\000\000\041\000\040\000\
\041\000\041\000\040\000\041\000\000\000\040\000\040\000\040\000\
\040\000\000\000\000\000\040\000\040\000\040\000\000\000\000\000\
\040\000\000\000\040\000\033\000\040\000\040\000\033\000\040\000\
\000\000\033\000\033\000\033\000\033\000\000\000\000\000\033\000\
\033\000\033\000\000\000\000\000\033\000\000\000\033\000\042\000\
\033\000\033\000\042\000\033\000\000\000\042\000\042\000\042\000\
\042\000\000\000\000\000\042\000\042\000\042\000\000\000\000\000\
\042\000\000\000\042\000\000\000\042\000\042\000\000\000\042\000\
\000\000\000\000\000\000\000\000\039\000\000\000\000\000\039\000\
\000\000\000\000\039\000\039\000\039\000\039\000\000\000\000\000\
\039\000\000\000\039\000\000\000\000\000\039\000\000\000\039\000\
\090\000\039\000\039\000\000\000\039\000\000\000\036\000\037\000\
\038\000\039\000\040\000\041\000\042\000\043\000\044\000\045\000\
\046\000\047\000\091\000\000\000\000\000\000\000\000\000\000\000\
\036\000\037\000\038\000\039\000\040\000\041\000\042\000\043\000\
\044\000\045\000\046\000\047\000\036\000\037\000\038\000\039\000\
\040\000\041\000\042\000\043\000\044\000\045\000\046\000\047\000"

let yycheck = "\004\000\
\000\000\012\001\019\001\008\000\009\000\010\000\002\001\024\001\
\001\000\020\001\002\001\001\001\008\001\003\001\004\001\011\001\
\006\001\007\001\010\001\011\001\019\001\019\001\042\001\013\001\
\002\001\024\001\024\001\032\000\018\001\034\000\008\001\042\001\
\010\001\011\001\019\001\019\001\005\001\031\001\032\001\000\000\
\030\001\026\001\026\001\048\000\049\000\050\000\051\000\028\001\
\053\000\054\000\055\000\041\001\042\001\043\001\059\000\021\001\
\042\001\062\000\024\001\064\000\029\001\030\001\031\001\032\001\
\033\001\034\001\035\001\036\001\037\001\038\001\039\001\040\001\
\019\001\021\001\079\000\019\001\024\001\002\001\000\000\026\001\
\024\001\086\000\087\000\008\001\042\001\010\001\011\001\092\000\
\019\001\022\001\000\000\019\001\025\001\024\001\002\001\028\001\
\024\001\102\000\028\001\042\001\008\001\106\000\010\001\011\001\
\018\001\010\001\020\001\042\001\022\001\021\001\115\000\009\001\
\024\001\042\001\021\001\018\001\035\001\024\001\035\001\124\000\
\042\001\017\001\017\001\027\001\042\001\027\001\042\001\042\001\
\027\001\000\000\008\001\029\001\030\001\031\001\032\001\033\001\
\034\001\035\001\036\001\037\001\038\001\039\001\040\001\015\001\
\035\001\042\001\035\001\022\000\098\000\000\000\110\000\089\000\
\026\000\255\255\255\255\014\001\255\255\029\001\030\001\031\001\
\032\001\033\001\034\001\035\001\036\001\037\001\038\001\039\001\
\040\001\000\000\029\001\030\001\031\001\032\001\033\001\034\001\
\035\001\036\001\037\001\038\001\039\001\040\001\016\001\255\255\
\255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
\255\255\255\255\255\255\029\001\030\001\031\001\032\001\033\001\
\034\001\035\001\036\001\037\001\038\001\039\001\040\001\255\255\
\255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\016\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\000\000\029\001\030\001\
\031\001\032\001\033\001\034\001\035\001\036\001\037\001\038\001\
\039\001\040\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\255\255\255\255\005\001\255\255\255\255\
\008\001\009\001\010\001\011\001\255\255\000\000\014\001\015\001\
\016\001\255\255\255\255\019\001\255\255\021\001\255\255\023\001\
\024\001\025\001\026\001\255\255\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\035\001\036\001\037\001\038\001\039\001\
\040\001\002\001\000\000\255\255\005\001\255\255\255\255\008\001\
\009\001\010\001\011\001\255\255\255\255\014\001\015\001\016\001\
\255\255\255\255\019\001\255\255\021\001\255\255\023\001\024\001\
\255\255\026\001\255\255\255\255\029\001\030\001\031\001\032\001\
\033\001\034\001\035\001\036\001\037\001\038\001\039\001\040\001\
\002\001\255\255\255\255\005\001\255\255\255\255\008\001\009\001\
\010\001\011\001\255\255\255\255\014\001\015\001\016\001\255\255\
\255\255\019\001\255\255\021\001\255\255\023\001\024\001\255\255\
\026\001\255\255\255\255\029\001\030\001\255\255\255\255\033\001\
\034\001\035\001\036\001\037\001\038\001\039\001\040\001\029\001\
\030\001\031\001\032\001\033\001\034\001\035\001\036\001\037\001\
\038\001\039\001\040\001\002\001\255\255\255\255\005\001\255\255\
\255\255\008\001\009\001\010\001\011\001\255\255\255\255\014\001\
\015\001\016\001\255\255\255\255\019\001\255\255\021\001\002\001\
\023\001\024\001\005\001\026\001\255\255\008\001\009\001\010\001\
\011\001\255\255\255\255\014\001\015\001\016\001\255\255\255\255\
\019\001\255\255\021\001\002\001\023\001\024\001\005\001\026\001\
\255\255\008\001\009\001\010\001\011\001\255\255\255\255\014\001\
\015\001\016\001\255\255\255\255\019\001\255\255\021\001\002\001\
\023\001\024\001\005\001\026\001\255\255\008\001\009\001\010\001\
\011\001\255\255\255\255\014\001\015\001\016\001\255\255\255\255\
\019\001\255\255\021\001\002\001\023\001\024\001\005\001\026\001\
\255\255\008\001\009\001\010\001\011\001\255\255\255\255\014\001\
\015\001\016\001\255\255\255\255\019\001\255\255\021\001\002\001\
\023\001\024\001\005\001\026\001\255\255\008\001\009\001\010\001\
\011\001\255\255\255\255\014\001\015\001\016\001\255\255\255\255\
\019\001\255\255\021\001\002\001\023\001\024\001\005\001\026\001\
\255\255\008\001\009\001\010\001\011\001\255\255\255\255\014\001\
\015\001\016\001\255\255\255\255\019\001\255\255\021\001\002\001\
\023\001\024\001\005\001\026\001\255\255\008\001\009\001\010\001\
\011\001\255\255\255\255\014\001\015\001\016\001\255\255\255\255\
\019\001\255\255\021\001\255\255\023\001\024\001\255\255\026\001\
\255\255\255\255\255\255\255\255\002\001\255\255\255\255\005\001\
\255\255\255\255\008\001\009\001\010\001\011\001\255\255\255\255\
\014\001\255\255\016\001\255\255\255\255\019\001\255\255\021\001\
\023\001\023\001\024\001\255\255\026\001\255\255\029\001\030\001\
\031\001\032\001\033\001\034\001\035\001\036\001\037\001\038\001\
\039\001\040\001\023\001\255\255\255\255\255\255\255\255\255\255\
\029\001\030\001\031\001\032\001\033\001\034\001\035\001\036\001\
\037\001\038\001\039\001\040\001\029\001\030\001\031\001\032\001\
\033\001\034\001\035\001\036\001\037\001\038\001\039\001\040\001"

let yynames_const = "\
  EOF\000\
  NIL\000\
  VAR\000\
  WHILE\000\
  FOR\000\
  TO\000\
  BREAK\000\
  LET\000\
  IN\000\
  END\000\
  FUNCTION\000\
  TYPE\000\
  ARRAY\000\
  IF\000\
  THEN\000\
  ELSE\000\
  DO\000\
  OF\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  LBRACKET\000\
  RBRACKET\000\
  COMMA\000\
  DOT\000\
  SEMICOLON\000\
  COLON\000\
  ASSIGN\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  OR\000\
  AND\000\
  EQUAL\000\
  NOTEQUAL\000\
  LT\000\
  LE\000\
  GT\000\
  GE\000\
  "

let yynames_block = "\
  STRING\000\
  ID\000\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 45 "parser.mly"
          ( _1 )
# 412 "parser.ml"
               : Absyn.exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "parser.mly"
        ( [] )
# 418 "parser.ml"
               : 'decs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'dec) in
    Obj.repr(
# 49 "parser.mly"
        ( _1 :: [] )
# 425 "parser.ml"
               : 'decs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'dec) in
    Obj.repr(
# 50 "parser.mly"
             ( _2 :: _1 )
# 433 "parser.ml"
               : 'decs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tydec) in
    Obj.repr(
# 53 "parser.mly"
          ( _1 )
# 440 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vardec) in
    Obj.repr(
# 54 "parser.mly"
           ( _1 )
# 447 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fundecs) in
    Obj.repr(
# 55 "parser.mly"
            ( Absyn.FunctionDec(_1) )
# 454 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 58 "parser.mly"
                     ( Absyn.TypeDec{name = Symbol.symbol _2; ty = _4; pos = mk_pos 1} )
# 462 "parser.ml"
               : 'tydec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 61 "parser.mly"
       ( Absyn.NameTy(Symbol.symbol _1, mk_pos 1) )
# 469 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'tyfields) in
    Obj.repr(
# 62 "parser.mly"
                           ( Absyn.RecordTy( _2 ) )
# 476 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 63 "parser.mly"
                         ( Absyn.ArrayTy(Symbol.symbol _3, mk_pos 1) )
# 484 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "parser.mly"
            ( [] )
# 490 "parser.ml"
               : 'tyfields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tyfield) in
    Obj.repr(
# 67 "parser.mly"
            ( _1 :: [] )
# 497 "parser.ml"
               : 'tyfields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'tyfields) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tyfield) in
    Obj.repr(
# 68 "parser.mly"
                           ( _3 :: _1 )
# 505 "parser.ml"
               : 'tyfields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 71 "parser.mly"
                ( {name = Symbol.symbol _1; escape = ref true; typ = Symbol.symbol _3; pos = mk_pos 1} )
# 513 "parser.ml"
               : 'tyfield))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 73 "parser.mly"
                      ( Absyn.VarDec{name = Symbol.symbol _2; escape = ref true; typ = None; init = _4; pos = mk_pos 1} )
# 521 "parser.ml"
               : 'vardec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fundec) in
    Obj.repr(
# 76 "parser.mly"
                   ( _1 :: [] )
# 528 "parser.ml"
               : 'fundecs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fundecs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fundec) in
    Obj.repr(
# 77 "parser.mly"
                   ( _2 :: _1 )
# 536 "parser.ml"
               : 'fundecs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'tyfields) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'resultty) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 79 "parser.mly"
                                                          ( {name = Symbol.symbol _2; params = _4; result = _6; body = _8; pos = mk_pos 1} )
# 546 "parser.ml"
               : 'fundec))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser.mly"
          ( None )
# 552 "parser.ml"
               : 'resultty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 83 "parser.mly"
              ( Some(Symbol.symbol _2, mk_pos 2) )
# 559 "parser.ml"
               : 'resultty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 85 "parser.mly"
       ( Absyn.Ident{name = Symbol.symbol _1; pos = mk_pos 1} )
# 566 "parser.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lvalue) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 86 "parser.mly"
                  ( Absyn.RecordAccess{record = _1; name = Symbol.symbol _3; pos = mk_pos 1} )
# 574 "parser.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'lvalue) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 87 "parser.mly"
                                 ( Absyn.ArrayAccess{array = _1; exp = _3; pos = mk_pos 1} )
# 582 "parser.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
        ( Absyn.NilExp(mk_pos 1) )
# 588 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 91 "parser.mly"
           ( Absyn.LValue{l = _1; pos = mk_pos 1} )
# 595 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expseq) in
    Obj.repr(
# 92 "parser.mly"
                         ( Absyn.SeqExp(_2) )
# 602 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 93 "parser.mly"
        ( Absyn.IntExp(_1) )
# 609 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 94 "parser.mly"
           ( Absyn.StringExp(_1, mk_pos 1) )
# 616 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 95 "parser.mly"
              ( Absyn.OpExp{left = Absyn.IntExp(0); op = Absyn.MinusOp; right = _2; pos = mk_pos 1} )
# 623 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'records) in
    Obj.repr(
# 97 "parser.mly"
                             ( Absyn.RecordExp{rec_exp_fields = _3; typ = Symbol.symbol _1; pos = mk_pos 1} )
# 631 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'funargs) in
    Obj.repr(
# 99 "parser.mly"
                             ( Absyn.CallExp{func = Symbol.symbol _1; args = _3; pos = mk_pos 1} )
# 639 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 100 "parser.mly"
                                    ( Absyn.ArrayExp{typ = Symbol.symbol _1; size = _3; init = _6; pos = mk_pos 1} )
# 648 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 101 "parser.mly"
               ( Absyn.OpExp{left = _1; op = _2; right = _3; pos = mk_pos 1} )
# 657 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'compar) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 102 "parser.mly"
                   ( Absyn.OpExp{left = _1; op = _2; right = _3; pos = mk_pos 1} )
# 666 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'boolean) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 103 "parser.mly"
                    ( Absyn.OpExp{left = _1; op = _2; right = _3; pos = mk_pos 1} )
# 675 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'decs) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 104 "parser.mly"
                        ( Absyn.LetExp{decs = _2; body = _4; pos = mk_pos 1} )
# 683 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lvalue) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 105 "parser.mly"
                      ( Absyn.AssignExp{lvalue = _1; exp = _3; pos = mk_pos 1} )
# 691 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 106 "parser.mly"
                    ( Absyn.IfExp{test = _2; then' = _4; else' = None; pos = mk_pos 1} )
# 699 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 107 "parser.mly"
                             ( Absyn.IfExp{test = _2; then' = _4; else' = Some(_6); pos = mk_pos 1} )
# 708 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 108 "parser.mly"
                     ( Absyn.WhileExp{test = _2; body = _4; pos = mk_pos 1} )
# 716 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 109 "parser.mly"
                                    ( Absyn.ForExp{var = Symbol.symbol _2; escape = ref true; lo = _4; hi = _6; body = _8; pos = mk_pos 1} )
# 726 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "parser.mly"
          ( Absyn.BreakExp (mk_pos 1) )
# 732 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 113 "parser.mly"
        ( (_1, mk_pos 1) :: [] )
# 739 "parser.ml"
               : 'expseq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expseq) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 114 "parser.mly"
                         ( (_3, Parsing.rhs_start_pos 3) :: _1 )
# 747 "parser.ml"
               : 'expseq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'record) in
    Obj.repr(
# 117 "parser.mly"
           ( _1 :: [] )
# 754 "parser.ml"
               : 'records))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'records) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'record) in
    Obj.repr(
# 118 "parser.mly"
                         ( _3 :: _1 )
# 762 "parser.ml"
               : 'records))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 121 "parser.mly"
                 ( (Symbol.symbol _1, _3, mk_pos 1) )
# 770 "parser.ml"
               : 'record))
; (fun __caml_parser_env ->
    Obj.repr(
# 123 "parser.mly"
        ( [] )
# 776 "parser.ml"
               : 'funargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 124 "parser.mly"
        ( _1 :: [] )
# 783 "parser.ml"
               : 'funargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'funargs) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 125 "parser.mly"
                      ( _3 :: _1 )
# 791 "parser.ml"
               : 'funargs))
; (fun __caml_parser_env ->
    Obj.repr(
# 128 "parser.mly"
         ( Absyn.PlusOp )
# 797 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    Obj.repr(
# 129 "parser.mly"
          ( Absyn.MinusOp )
# 803 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    Obj.repr(
# 130 "parser.mly"
          ( Absyn.TimesOp )
# 809 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    Obj.repr(
# 131 "parser.mly"
           ( Absyn.DivideOp )
# 815 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    Obj.repr(
# 134 "parser.mly"
       ( Absyn.LtOp )
# 821 "parser.ml"
               : 'compar))
; (fun __caml_parser_env ->
    Obj.repr(
# 135 "parser.mly"
       ( Absyn.LeOp )
# 827 "parser.ml"
               : 'compar))
; (fun __caml_parser_env ->
    Obj.repr(
# 136 "parser.mly"
       ( Absyn.GtOp )
# 833 "parser.ml"
               : 'compar))
; (fun __caml_parser_env ->
    Obj.repr(
# 137 "parser.mly"
       ( Absyn.GeOp )
# 839 "parser.ml"
               : 'compar))
; (fun __caml_parser_env ->
    Obj.repr(
# 138 "parser.mly"
          ( Absyn.EqOp )
# 845 "parser.ml"
               : 'compar))
; (fun __caml_parser_env ->
    Obj.repr(
# 139 "parser.mly"
             ( Absyn.NeqOp )
# 851 "parser.ml"
               : 'compar))
; (fun __caml_parser_env ->
    Obj.repr(
# 142 "parser.mly"
        ( Absyn.AndOp )
# 857 "parser.ml"
               : 'boolean))
; (fun __caml_parser_env ->
    Obj.repr(
# 143 "parser.mly"
       ( Absyn.OrOp )
# 863 "parser.ml"
               : 'boolean))
(* Entry prog *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Absyn.exp)
;;
