let () =
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.prog Lexer.puppy lexbuf in
  let {Semant.exp=exp; Semant.ty=ty} = Semant.transExp Env.base_venv Env.base_tenv ast in
  Printf.printf "%b %b\n" (exp = Translate.exp) (ty = Types.Int)
