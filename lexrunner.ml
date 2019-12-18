open Puppy_parser

let rec lexing_puppy lexbuf tokens =
    let token = puppy lexbuf in
    match token with
      | EOF -> token::tokens
      | _   -> lexing_puppy lexbuf (token::tokens)

  let rec print_tokens tokens =
    let print_token = function
    | ASSIGN            -> printf "ASSIGN, "
    | EQUALS            -> printf "EQUALS, "
    | SEMICOLON         -> printf "SEMICOLON, "
    | COMMA             -> printf "COMMA, "
    | PLUS              -> printf "PLUS, "
    | MINUS             -> printf "MINUS, "
    | TIMES             -> printf "TIMES, "
    | DIVIDE            -> printf "DIVIDE, "
    | WHILE             -> printf "WHILE, "
    | FOR               -> printf "FOR, "
    | TO                -> printf "TO, "
    | BREAK             -> printf "BREAK, "
    | LET               -> printf "LET, "
    | IN                -> printf "IN, "
    | END               -> printf "END, "
    | FUNCTION          -> printf "FUNCTION, "
    | VAR               -> printf "VAR, "
    | TYPE              -> printf "TYPE, "
    | ARRAY             -> printf "ARRAY, "
    | IF                -> printf "IF, "
    | THEN              -> printf "THEN, "
    | ELSE              -> printf "ELSE, "
    | DO                -> printf "DO, "
    | OF                -> printf "OF, "
    | NIL               -> printf "NIL, "
    | INT         dig   -> printf "INT(%d), "       dig
    | STRING      str   -> printf "STRING(%s), "    str
    | ID          id    -> printf "ID(%s), "        id
    | EOF               -> printf "EOF, "           in
  match tokens with
    | [] -> ()
    | _  -> print_token (List.hd tokens); print_tokens (List.tl tokens)

  let main () =
    let lexbuf = Lexing.from_channel stdin in
    let tokens = List.rev (lexing_puppy lexbuf []) in
    print_tokens tokens

(* Print any exception *)
  let _ = Printexc.print main ()
