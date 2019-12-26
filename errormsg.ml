open Printf

type errorType =
  | LexingError
  | SyntaxError
  | TypeError

exception Error of errorType * Lexing.position * string

let match_error = function
  | LexingError -> "Lexing error"
  | SyntaxError -> "Syntax error"
  | TypeError -> "Type error"

let raise_error err pos msg =
  raise @@ Error (err, pos, msg)

let display_error (error, pos , msg) =
  let lineNum = pos.Lexing.pos_lnum in
  let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  (sprintf "%s:\n\t line:%d, column: %d\n\t%s" (match_error error) lineNum col  msg)
