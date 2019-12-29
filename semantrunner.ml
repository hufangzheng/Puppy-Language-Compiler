let () =
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.prog Lexer.puppy lexbuf in
  ignore (Semant.transExp Env.base_venv Env.base_tenv ast)
