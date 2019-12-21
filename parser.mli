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

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Absyn.exp
