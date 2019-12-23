(* 用于判断两个record或array是否相等 *)
type unique = unit

type ty =
  | INT
  | STRING
  | RECORD of (Symbol.symbol * ty) list * unique
  | ARRAY of ty * unique
  | NIL
  | UNIT
  | NAME of Symbol.symbol * ty option ref
