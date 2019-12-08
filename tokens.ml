module Tokens =
  struct
    type pos   =   int * int
    type token =
      | ASSIGN     of pos
      | EQUALS     of pos
      | SEMICOLON  of pos
      | COMMA      of pos
      | PLUS       of pos
      | MINUS      of pos
      | TIMES      of pos
      | DIVIDE     of pos
      | WHILE      of pos
      | FOR        of pos
      | TO         of pos
      | BREAK      of pos
      | LET        of pos
      | IN         of pos
      | END        of pos
      | FUNCTION   of pos
      | VAR        of pos
      | TYPE       of pos
      | ARRAY      of pos
      | IF         of pos
      | THEN       of pos
      | ELSE       of pos
      | DO         of pos
      | OF         of pos
      | NIL        of pos
      | INT        of int * pos
      | STRING     of string * pos
      | ID         of string * pos
      | EOF        of pos
  end;;
