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
\001\000\003\000\004\000\004\000\004\000\005\000\008\000\008\000\
\008\000\009\000\009\000\009\000\010\000\006\000\007\000\007\000\
\011\000\012\000\012\000\013\000\013\000\013\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\014\000\
\014\000\015\000\015\000\020\000\016\000\016\000\016\000\017\000\
\017\000\017\000\017\000\018\000\018\000\018\000\018\000\018\000\
\018\000\019\000\019\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\004\000\001\000\003\000\
\005\000\000\000\001\000\003\000\003\000\004\000\001\000\002\000\
\008\000\000\000\002\000\001\000\003\000\004\000\001\000\003\000\
\001\000\001\000\002\000\004\000\004\000\006\000\003\000\003\000\
\003\000\003\000\004\000\006\000\004\000\008\000\001\000\001\000\
\003\000\001\000\003\000\003\000\000\000\001\000\003\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\039\000\000\000\000\000\000\000\
\026\000\000\000\025\000\060\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\048\000\049\000\050\000\051\000\059\000\058\000\056\000\057\000\
\052\000\053\000\054\000\055\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\024\000\000\000\000\000\
\000\000\000\000\000\000\042\000\000\000\000\000\000\000\000\000\
\000\000\021\000\000\000\000\000\000\000\000\000\000\000\029\000\
\000\000\000\000\028\000\000\000\000\000\022\000\000\000\000\000\
\000\000\000\000\043\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yydgoto = "\002\000\
\012\000\013\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\014\000\019\000\051\000\049\000\
\037\000\038\000\039\000\052\000"

let yysindex = "\013\000\
\014\255\000\000\014\255\218\254\000\000\014\255\014\255\014\255\
\000\000\031\255\000\000\000\000\085\000\247\254\106\255\249\254\
\070\255\171\255\237\254\227\254\014\255\236\254\014\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\014\255\014\255\014\255\014\255\
\252\254\014\255\014\255\014\255\014\255\000\000\014\255\171\255\
\004\255\254\254\037\255\000\000\158\255\171\255\171\255\171\255\
\216\255\000\000\171\255\171\255\057\255\198\255\171\255\000\000\
\014\255\014\255\000\000\236\254\018\255\000\000\014\255\014\255\
\171\255\171\255\000\000\014\255\132\255\171\255\171\255\014\255\
\171\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\001\000\000\000\000\000\000\000\037\000\000\000\000\000\
\000\000\242\254\000\000\073\000\006\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\010\255\
\000\000\000\000\000\000\000\000\000\000\121\000\134\000\147\000\
\000\000\000\000\160\000\173\000\000\000\225\000\245\254\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\012\255\039\255\000\000\000\000\000\000\186\000\199\000\000\000\
\212\000"

let yygindex = "\000\000\
\000\000\003\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\227\255"

let yytablesize = 507
let yytable = "\046\000\
\020\000\027\000\028\000\016\000\040\000\015\000\047\000\041\000\
\017\000\018\000\020\000\040\000\040\000\001\000\041\000\041\000\
\003\000\004\000\042\000\005\000\044\000\050\000\064\000\048\000\
\045\000\053\000\006\000\065\000\046\000\045\000\047\000\007\000\
\066\000\046\000\076\000\047\000\023\000\058\000\075\000\054\000\
\055\000\056\000\057\000\008\000\059\000\060\000\061\000\062\000\
\021\000\063\000\022\000\000\000\023\000\000\000\009\000\010\000\
\011\000\067\000\000\000\044\000\068\000\071\000\044\000\000\000\
\000\000\000\000\000\000\073\000\074\000\000\000\000\000\000\000\
\027\000\077\000\078\000\000\000\000\000\000\000\079\000\000\000\
\000\000\000\000\081\000\045\000\024\000\025\000\026\000\027\000\
\028\000\029\000\030\000\031\000\032\000\033\000\034\000\035\000\
\036\000\000\000\025\000\026\000\027\000\028\000\029\000\030\000\
\031\000\032\000\033\000\034\000\035\000\036\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\031\000\043\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\032\000\025\000\026\000\
\027\000\028\000\029\000\030\000\031\000\032\000\033\000\034\000\
\035\000\036\000\033\000\080\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\034\000\
\025\000\026\000\027\000\028\000\029\000\030\000\031\000\032\000\
\033\000\034\000\035\000\036\000\037\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\069\000\000\000\000\000\000\000\
\000\000\036\000\025\000\026\000\027\000\028\000\029\000\030\000\
\031\000\032\000\033\000\034\000\035\000\036\000\030\000\025\000\
\026\000\027\000\028\000\029\000\030\000\031\000\032\000\033\000\
\034\000\035\000\036\000\038\000\072\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\035\000\000\000\025\000\026\000\027\000\028\000\029\000\030\000\
\031\000\032\000\033\000\034\000\035\000\036\000\070\000\000\000\
\000\000\000\000\000\000\000\000\025\000\026\000\027\000\028\000\
\029\000\030\000\031\000\032\000\033\000\034\000\035\000\036\000\
\000\000\000\000\000\000\000\000\000\000\020\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\020\000\020\000\
\020\000\000\000\000\000\020\000\000\000\020\000\000\000\020\000\
\020\000\020\000\020\000\000\000\020\000\020\000\020\000\020\000\
\020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
\020\000\023\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\023\000\023\000\023\000\000\000\000\000\023\000\
\000\000\023\000\000\000\023\000\023\000\000\000\023\000\000\000\
\000\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
\023\000\023\000\023\000\023\000\023\000\027\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\027\000\027\000\
\027\000\000\000\000\000\027\000\000\000\027\000\000\000\027\000\
\027\000\000\000\027\000\000\000\000\000\027\000\027\000\000\000\
\000\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
\027\000\025\000\026\000\027\000\028\000\029\000\030\000\031\000\
\032\000\033\000\034\000\035\000\036\000\031\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\031\000\031\000\
\031\000\000\000\032\000\031\000\000\000\031\000\000\000\031\000\
\031\000\000\000\031\000\032\000\032\000\032\000\000\000\033\000\
\032\000\000\000\032\000\000\000\032\000\032\000\000\000\032\000\
\033\000\033\000\033\000\000\000\034\000\033\000\000\000\033\000\
\000\000\033\000\033\000\000\000\033\000\034\000\034\000\034\000\
\000\000\037\000\034\000\000\000\034\000\000\000\034\000\034\000\
\000\000\034\000\037\000\037\000\037\000\000\000\036\000\037\000\
\000\000\037\000\000\000\037\000\037\000\000\000\037\000\036\000\
\036\000\036\000\000\000\030\000\036\000\000\000\036\000\000\000\
\036\000\036\000\000\000\036\000\030\000\030\000\030\000\000\000\
\038\000\030\000\000\000\030\000\000\000\030\000\030\000\000\000\
\030\000\038\000\038\000\038\000\000\000\035\000\038\000\000\000\
\038\000\000\000\038\000\038\000\000\000\038\000\035\000\000\000\
\035\000\000\000\000\000\035\000\000\000\035\000\000\000\035\000\
\035\000\000\000\035\000"

let yycheck = "\019\001\
\000\000\031\001\032\001\042\001\019\001\003\000\026\001\019\001\
\006\000\007\000\008\000\026\001\022\001\001\000\026\001\025\001\
\003\001\004\001\028\001\006\001\028\001\042\001\019\001\021\000\
\019\001\023\000\013\001\024\001\019\001\024\001\019\001\018\001\
\035\001\024\001\017\001\024\001\000\000\042\001\068\000\037\000\
\038\000\039\000\040\000\030\001\042\000\043\000\044\000\045\000\
\018\001\047\000\020\001\255\255\022\001\255\255\041\001\042\001\
\043\001\021\001\255\255\021\001\024\001\005\001\024\001\255\255\
\255\255\255\255\255\255\065\000\066\000\255\255\255\255\255\255\
\000\000\071\000\072\000\255\255\255\255\255\255\076\000\255\255\
\255\255\255\255\080\000\014\001\000\000\029\001\030\001\031\001\
\032\001\033\001\034\001\035\001\036\001\037\001\038\001\039\001\
\040\001\255\255\029\001\030\001\031\001\032\001\033\001\034\001\
\035\001\036\001\037\001\038\001\039\001\040\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\000\016\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\000\000\029\001\030\001\
\031\001\032\001\033\001\034\001\035\001\036\001\037\001\038\001\
\039\001\040\001\000\000\016\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
\029\001\030\001\031\001\032\001\033\001\034\001\035\001\036\001\
\037\001\038\001\039\001\040\001\000\000\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\023\001\255\255\255\255\255\255\
\255\255\000\000\029\001\030\001\031\001\032\001\033\001\034\001\
\035\001\036\001\037\001\038\001\039\001\040\001\000\000\029\001\
\030\001\031\001\032\001\033\001\034\001\035\001\036\001\037\001\
\038\001\039\001\040\001\000\000\015\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\000\255\255\029\001\030\001\031\001\032\001\033\001\034\001\
\035\001\036\001\037\001\038\001\039\001\040\001\023\001\255\255\
\255\255\255\255\255\255\255\255\029\001\030\001\031\001\032\001\
\033\001\034\001\035\001\036\001\037\001\038\001\039\001\040\001\
\255\255\255\255\255\255\255\255\255\255\005\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\014\001\015\001\
\016\001\255\255\255\255\019\001\255\255\021\001\255\255\023\001\
\024\001\025\001\026\001\255\255\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\035\001\036\001\037\001\038\001\039\001\
\040\001\005\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\014\001\015\001\016\001\255\255\255\255\019\001\
\255\255\021\001\255\255\023\001\024\001\255\255\026\001\255\255\
\255\255\029\001\030\001\031\001\032\001\033\001\034\001\035\001\
\036\001\037\001\038\001\039\001\040\001\005\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\014\001\015\001\
\016\001\255\255\255\255\019\001\255\255\021\001\255\255\023\001\
\024\001\255\255\026\001\255\255\255\255\029\001\030\001\255\255\
\255\255\033\001\034\001\035\001\036\001\037\001\038\001\039\001\
\040\001\029\001\030\001\031\001\032\001\033\001\034\001\035\001\
\036\001\037\001\038\001\039\001\040\001\005\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\014\001\015\001\
\016\001\255\255\005\001\019\001\255\255\021\001\255\255\023\001\
\024\001\255\255\026\001\014\001\015\001\016\001\255\255\005\001\
\019\001\255\255\021\001\255\255\023\001\024\001\255\255\026\001\
\014\001\015\001\016\001\255\255\005\001\019\001\255\255\021\001\
\255\255\023\001\024\001\255\255\026\001\014\001\015\001\016\001\
\255\255\005\001\019\001\255\255\021\001\255\255\023\001\024\001\
\255\255\026\001\014\001\015\001\016\001\255\255\005\001\019\001\
\255\255\021\001\255\255\023\001\024\001\255\255\026\001\014\001\
\015\001\016\001\255\255\005\001\019\001\255\255\021\001\255\255\
\023\001\024\001\255\255\026\001\014\001\015\001\016\001\255\255\
\005\001\019\001\255\255\021\001\255\255\023\001\024\001\255\255\
\026\001\014\001\015\001\016\001\255\255\005\001\019\001\255\255\
\021\001\255\255\023\001\024\001\255\255\026\001\014\001\255\255\
\016\001\255\255\255\255\019\001\255\255\021\001\255\255\023\001\
\024\001\255\255\026\001"

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
# 369 "parser.ml"
               : Absyn.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'dec) in
    Obj.repr(
# 48 "parser.mly"
        ( _1 )
# 376 "parser.ml"
               : 'decs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tydec) in
    Obj.repr(
# 51 "parser.mly"
          ( _1 )
# 383 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vardec) in
    Obj.repr(
# 52 "parser.mly"
           ( _1 )
# 390 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fundecs) in
    Obj.repr(
# 53 "parser.mly"
            ( Absyn.FunctionDec(_1) )
# 397 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 56 "parser.mly"
                     ( Absyn.TypeDec{name = Symbol.symbol _2; ty = _4; pos = mk_pos 1} )
# 405 "parser.ml"
               : 'tydec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 59 "parser.mly"
       ( Absyn.NameTy(Symbol.symbol _1, mk_pos 1) )
# 412 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'tyfields) in
    Obj.repr(
# 60 "parser.mly"
                           ( Absyn.RecordTy( _2 ) )
# 419 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 61 "parser.mly"
                         ( Absyn.ArrayTy(Symbol.symbol _3, mk_pos 1) )
# 427 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "parser.mly"
            ( [] )
# 433 "parser.ml"
               : 'tyfields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tyfield) in
    Obj.repr(
# 65 "parser.mly"
            ( _1 :: [] )
# 440 "parser.ml"
               : 'tyfields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'tyfields) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tyfield) in
    Obj.repr(
# 66 "parser.mly"
                           ( _3 :: _1 )
# 448 "parser.ml"
               : 'tyfields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 69 "parser.mly"
                ( {name = Symbol.symbol _1; escape = ref true; typ = Symbol.symbol _3; pos = mk_pos 1} )
# 456 "parser.ml"
               : 'tyfield))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 71 "parser.mly"
                      ( Absyn.VarDec{name = Symbol.symbol _2; escape = ref true; typ = None; init = _4; pos = mk_pos 1} )
# 464 "parser.ml"
               : 'vardec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fundec) in
    Obj.repr(
# 74 "parser.mly"
                   ( _1 :: [] )
# 471 "parser.ml"
               : 'fundecs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fundecs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fundec) in
    Obj.repr(
# 75 "parser.mly"
                   ( _2 :: _1 )
# 479 "parser.ml"
               : 'fundecs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'tyfields) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'resultty) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 77 "parser.mly"
                                                          ( {name = Symbol.symbol _2; params = _4; result = _6; body = _8; pos = mk_pos 1} )
# 489 "parser.ml"
               : 'fundec))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
          ( None )
# 495 "parser.ml"
               : 'resultty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 81 "parser.mly"
              ( Some(Symbol.symbol _2, mk_pos 2) )
# 502 "parser.ml"
               : 'resultty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 83 "parser.mly"
       ( Absyn.Ident{name = Symbol.symbol _1; pos = mk_pos 1} )
# 509 "parser.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lvalue) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 84 "parser.mly"
                  ( Absyn.RecordAccess{record = _1; name = Symbol.symbol _3; pos = mk_pos 1} )
# 517 "parser.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'lvalue) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 85 "parser.mly"
                                 ( Absyn.ArrayAccess{array = _1; exp = _3; pos = mk_pos 1} )
# 525 "parser.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 88 "parser.mly"
           ( Absyn.LValue{l = _1; pos = mk_pos 1} )
# 532 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expseq) in
    Obj.repr(
# 89 "parser.mly"
                         ( Absyn.SeqExp(_2) )
# 539 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 90 "parser.mly"
        ( Absyn.IntExp(_1) )
# 546 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 91 "parser.mly"
           ( Absyn.StringExp(_1, mk_pos 1) )
# 553 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 92 "parser.mly"
              ( Absyn.OpExp{left = Absyn.IntExp(0); op = Absyn.MinusOp; right = _2; pos = mk_pos 1} )
# 560 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'records) in
    Obj.repr(
# 94 "parser.mly"
                             ( Absyn.RecordExp{rec_exp_fields = _3; typ = Symbol.symbol _1; pos = mk_pos 1} )
# 568 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'funargs) in
    Obj.repr(
# 96 "parser.mly"
                             ( Absyn.CallExp{func = Symbol.symbol _1; args = _3; pos = mk_pos 1} )
# 576 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 97 "parser.mly"
                                    ( Absyn.ArrayExp{typ = Symbol.symbol _1; size = _3; init = _6; pos = mk_pos 1} )
# 585 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 98 "parser.mly"
               ( Absyn.OpExp{left = _1; op = _2; right = _3; pos = mk_pos 1} )
# 594 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'compar) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 99 "parser.mly"
                   ( Absyn.OpExp{left = _1; op = _2; right = _3; pos = mk_pos 1} )
# 603 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'boolean) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 100 "parser.mly"
                    ( Absyn.OpExp{left = _1; op = _2; right = _3; pos = mk_pos 1} )
# 612 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lvalue) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 101 "parser.mly"
                      ( Absyn.AssignExp{lvalue = _1; exp = _3; pos = mk_pos 1} )
# 620 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 102 "parser.mly"
                    ( Absyn.IfExp{test = _2; then' = _4; else' = None; pos = mk_pos 1} )
# 628 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 103 "parser.mly"
                             ( Absyn.IfExp{test = _2; then' = _4; else' = Some(_6); pos = mk_pos 1} )
# 637 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 104 "parser.mly"
                     ( Absyn.WhileExp{test = _2; body = _4; pos = mk_pos 1} )
# 645 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 105 "parser.mly"
                                    ( Absyn.ForExp{var = Symbol.symbol _2; escape = ref true; lo = _4; hi = _6; body = _8; pos = mk_pos 1} )
# 655 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 106 "parser.mly"
          ( Absyn.BreakExp (mk_pos 1) )
# 661 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 109 "parser.mly"
        ( (_1, mk_pos 1) :: [] )
# 668 "parser.ml"
               : 'expseq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expseq) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 110 "parser.mly"
                         ( (_3, Parsing.rhs_start_pos 3) :: _1 )
# 676 "parser.ml"
               : 'expseq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'record) in
    Obj.repr(
# 113 "parser.mly"
           ( _1 :: [] )
# 683 "parser.ml"
               : 'records))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'records) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'record) in
    Obj.repr(
# 114 "parser.mly"
                         ( _3 :: _1 )
# 691 "parser.ml"
               : 'records))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 117 "parser.mly"
                 ( (Symbol.symbol _1, _3, mk_pos 1) )
# 699 "parser.ml"
               : 'record))
; (fun __caml_parser_env ->
    Obj.repr(
# 119 "parser.mly"
        ( [] )
# 705 "parser.ml"
               : 'funargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 120 "parser.mly"
        ( _1 :: [] )
# 712 "parser.ml"
               : 'funargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'funargs) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 121 "parser.mly"
                      ( _3 :: _1 )
# 720 "parser.ml"
               : 'funargs))
; (fun __caml_parser_env ->
    Obj.repr(
# 124 "parser.mly"
         ( Absyn.PlusOp )
# 726 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    Obj.repr(
# 125 "parser.mly"
          ( Absyn.MinusOp )
# 732 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    Obj.repr(
# 126 "parser.mly"
          ( Absyn.TimesOp )
# 738 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    Obj.repr(
# 127 "parser.mly"
           ( Absyn.DivideOp )
# 744 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    Obj.repr(
# 130 "parser.mly"
       ( Absyn.LtOp )
# 750 "parser.ml"
               : 'compar))
; (fun __caml_parser_env ->
    Obj.repr(
# 131 "parser.mly"
       ( Absyn.LeOp )
# 756 "parser.ml"
               : 'compar))
; (fun __caml_parser_env ->
    Obj.repr(
# 132 "parser.mly"
       ( Absyn.GtOp )
# 762 "parser.ml"
               : 'compar))
; (fun __caml_parser_env ->
    Obj.repr(
# 133 "parser.mly"
       ( Absyn.GeOp )
# 768 "parser.ml"
               : 'compar))
; (fun __caml_parser_env ->
    Obj.repr(
# 134 "parser.mly"
          ( Absyn.EqOp )
# 774 "parser.ml"
               : 'compar))
; (fun __caml_parser_env ->
    Obj.repr(
# 135 "parser.mly"
             ( Absyn.NeqOp )
# 780 "parser.ml"
               : 'compar))
; (fun __caml_parser_env ->
    Obj.repr(
# 138 "parser.mly"
        ( Absyn.AndOp )
# 786 "parser.ml"
               : 'boolean))
; (fun __caml_parser_env ->
    Obj.repr(
# 139 "parser.mly"
       ( Absyn.OrOp )
# 792 "parser.ml"
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
