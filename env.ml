type access = unit
type ty = Types.ty
type enventry =
  | VarEntry of { ty: ty }
  | FunEntry of { formals: ty list; result: ty }

(* base_tenv类型为'a table，用于存储类型环境 *)
let base_tenv =
  List.fold_left
  (fun table (sym, ty) -> Symbol.enter table sym ty)
  Symbol.empty
  [Symbol.symbol "int", Types.INT; Symbol.symbol "string", Types.STRING]

(* base_venv类型为'a table，用于存储变量环境 *)
let base_venv =
  List.fold_left
  (fun table (sym, entry) -> Symbol.enter table sym entry)
  Symbol.empty
  [Symbol.symbol "print", FunEntry{formals = [Types.STRING]; result = Types.UNIT};
   Symbol.symbol "flush", FunEntry{formals = []; result = Types.UNIT}]
