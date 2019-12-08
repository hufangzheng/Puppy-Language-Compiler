{
  open Printf
  open Tokens
  open Errormsg

  let lineNum = Errormsg.lineNum
  let linePos = Errormsg.linePos
  let comment_depth = ref 0               (* The depth of nested comments *)

  let create_pos lexbuf = (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)
(* let eof() = let pos = hd(!linePos) in Token.EOF(pos, pos) *)
}

let digits = ['0'-'9']+
let puppy_id = ['_'  'a'-'z'  'A'-'Z']['_' 'a'-'z' 'A'-'Z' '0'-'9']*
let puppy_string = ['<']_*['>']
let witespace = [' ' '\t']+

rule puppy = parse
  | witespace                 { puppy lexbuf }
  | ":="                      { Tokens.ASSIGN(create_pos lexbuf) }
  | "="                       { Tokens.EQUALS(create_pos lexbuf) }
  | ";"                       { Tokens.SEMICOLON(create_pos lexbuf) }
  | ","                       { Tokens.COMMA(create_pos lexbuf) }
  | "+"                       { Tokens.PLUS(create_pos lexbuf) }
  | "-"                       { Tokens.MINUS(create_pos lexbuf) }
  | "*"                       { Tokens.TIMES(create_pos lexbuf) }
  | "/"                       { Tokens.DIVIDE(create_pos lexbuf) }
  | "while"                   { Tokens.WHILE(create_pos lexbuf) }
  | "for"                     { Tokens.FOR(create_pos lexbuf) }
  | "to"                      { Tokens.TO(create_pos lexbuf) }
  | "break"                   { Tokens.BREAK(create_pos lexbuf) }
  | "let"                     { Tokens.LET(create_pos lexbuf) }
  | "in"                      { Tokens.IN(create_pos lexbuf) }
  | "end"                     { Tokens.END(create_pos lexbuf) }
  | "function"                { Tokens.FUNCTION(create_pos lexbuf) }
  | "var"                     { Tokens.VAR(create_pos lexbuf) }
  | "type"                    { Tokens.TYPE(create_pos lexbuf) }
  | "array"                   { Tokens.ARRAY(create_pos lexbuf) }
  | "if"                      { Tokens.IF(create_pos lexbuf) }
  | "then"                    { Tokens.THEN(create_pos lexbuf) }
  | "else"                    { Tokens.ELSE(create_pos lexbuf) }
  | "do"                      { Tokens.DO(create_pos lexbuf) }
  | "of"                      { Tokens.OF(create_pos lexbuf) }
  | "nil"                     { Tokens.NIL(create_pos lexbuf) }
  | digits as num             { Tokens.INT(int_of_string num, (create_pos lexbuf)) }
  | puppy_string as str       { Tokens.STRING(str, (create_pos lexbuf)) }
  | '\n'                      { incr lineNum; linePos := (Lexing.lexeme_start lexbuf) :: !linePos; puppy lexbuf }
  | "/*"                      { incr comment_depth; puppy_comment lexbuf }
  | puppy_id as id            { Tokens.ID(id, (create_pos lexbuf)) }
  | _ as c                    { printf "Unrecognized character: %c" c; puppy lexbuf }
  | eof                       { Tokens.EOF(create_pos lexbuf) }
and puppy_comment = parse
  | '\n'                      { incr lineNum; linePos := (Lexing.lexeme_start lexbuf) :: !linePos; puppy_comment lexbuf }
  | "//"_*                    { puppy lexbuf }
  | "/*"                      { incr comment_depth; puppy_comment lexbuf }
  | "*/"                      { decr comment_depth; match !comment_depth with 0 -> puppy lexbuf | _ -> puppy_comment lexbuf }
  | _                         { puppy_comment lexbuf }

{
  let rec lexing_puppy lexbuf tokens =
    let token = puppy lexbuf in
    match token with
      | Tokens.EOF (p1, p2) -> token::tokens
      | _   -> lexing_puppy lexbuf (token::tokens)

  let rec print_tokens tokens =
    let print_token = function
    | Tokens.ASSIGN      pos           -> printf "ASSIGN, "
    | Tokens.EQUALS      pos           -> printf "EQUALS, "
    | Tokens.SEMICOLON   pos           -> printf "SEMICOLON, "
    | Tokens.COMMA       pos           -> printf "COMMA, "
    | Tokens.PLUS        pos           -> printf "PLUS, "
    | Tokens.MINUS       pos           -> printf "MINUS, "
    | Tokens.TIMES       pos           -> printf "TIMES, "
    | Tokens.DIVIDE      pos           -> printf "DIVIDE, "
    | Tokens.WHILE       pos           -> printf "WHILE, "
    | Tokens.FOR         pos           -> printf "FOR, "
    | Tokens.TO          pos           -> printf "TO, "
    | Tokens.BREAK       pos           -> printf "BREAK, "
    | Tokens.LET         pos           -> printf "LET, "
    | Tokens.IN          pos           -> printf "IN, "
    | Tokens.END         pos           -> printf "END, "
    | Tokens.FUNCTION    pos           -> printf "FUNCTION, "
    | Tokens.VAR         pos           -> printf "VAR, "
    | Tokens.TYPE        pos           -> printf "TYPE, "
    | Tokens.ARRAY       pos           -> printf "ARRAY, "
    | Tokens.IF          pos           -> printf "IF, "
    | Tokens.THEN        pos           -> printf "THEN, "
    | Tokens.ELSE        pos           -> printf "ELSE, "
    | Tokens.DO          pos           -> printf "DO, "
    | Tokens.OF          pos           -> printf "OF, "
    | Tokens.NIL         pos           -> printf "NIL, "
    | Tokens.INT         (dig, pos)    -> printf "INT(%d), "       dig
    | Tokens.STRING      (str, pos)    -> printf "STRING(%s), "    str
    | Tokens.ID          (id, pos)     -> printf "ID(%s), "        id
    | Tokens.EOF         pos           -> printf "EOF, "           in
  match tokens with
    | [] -> ()
    | _  -> print_token (List.hd tokens); print_tokens (List.tl tokens)

  let main () =
    let lexbuf = Lexing.from_channel stdin in
    let tokens = List.rev (lexing_puppy lexbuf []) in
    print_tokens tokens

(* Print any exception *)
  let _ = Printexc.print main ()
}
