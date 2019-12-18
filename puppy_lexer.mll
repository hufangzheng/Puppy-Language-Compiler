{
  open Printf
  open Tokens
  open Errormsg
  open Puppy_parser

  let lineNum = Errormsg.lineNum
  let linePos = Errormsg.linePos
  let comment_depth = ref 0               (* The depth of nested comments *)

  let create_pos lexbuf = (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)
  let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
                                  Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
                                  Lexing.pos_bol = pos.Lexing.pos_cnum; }
}

let digits = ['0'-'9']+
let puppy_id = ['_'  'a'-'z'  'A'-'Z']['_' 'a'-'z' 'A'-'Z' '0'-'9']*
let puppy_string = ['<']_*['>']
let witespace = [' ' '\t']+

rule puppy = parse
  | witespace                 { puppy lexbuf }
  | ":="                      { ASSIGN }
  | "="                       { EQUALS }
  | ";"                       { SEMICOLON }
  | ","                       { COMMA }
  | "+"                       { PLUS }
  | "-"                       { MINUS }
  | "*"                       { TIMES }
  | "/"                       { DIVIDE }
  | "while"                   { WHILE }
  | "for"                     { FOR }
  | "to"                      { TO }
  | "break"                   { BREAK }
  | "let"                     { LET }
  | "in"                      { IN }
  | "end"                     { END }
  | "function"                { FUNCTION }
  | "var"                     { VAR }
  | "type"                    { TYPE }
  | "array"                   { ARRAY }
  | "if"                      { IF }
  | "then"                    { THEN }
  | "else"                    { ELSE }
  | "do"                      { DO }
  | "of"                      { OF }
  | "nil"                     { NIL }
  | digits as num             { INT(int_of_string num) }
  | puppy_string as str       { STRING(str) }
  | '\n'                      { incr_linenum lexbuf; puppy lexbuf }
  | "/*"                      { incr comment_depth; puppy_comment lexbuf }
  | puppy_id as id            { ID(id) }
  | _ as c                    { printf "Unrecognized character: %c" c; puppy lexbuf }
  | eof                       { EOF }
and puppy_comment depth = parse
  | '\n'                      { incr_linenum lexbuf; puppy_comment lexbuf }
  | "//"_*                    { puppy lexbuf }
  | "/*"                      { puppy_comment (depth + 1) lexbuf }
  | "*/"                      { if depth = 0 then puppy lexbuf else puppy_comment (depth - 1) lexbuf }
  | _                         { puppy_comment lexbuf }
