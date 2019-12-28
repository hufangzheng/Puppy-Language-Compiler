open Printf

(* 用于判断两个record或array是否相等 *)
type unique = unit ref

type ty =
  | INT
  | STRING
  | RECORD of (Symbol.symbol * ty) list * unique
  | ARRAY of ty * unique
  | NIL
  | UNIT
  | NAME of Symbol.symbol * ty option ref

(* 将类型转换为string，用于输出错误信息 *)
let rec string_of_ty = function
  | INT                     -> "int"
  | STRING                  -> "string"
  | NIL                     -> "nil"
  | UNIT                    -> "unit"
  | RECORD (fields, unique) -> sprintf "record{%s}" (String.concat ";" (List.map string_of_field fields))
  | ARRAY (ty, _)           -> sprintf "array of %s" (string_of_ty ty)
  | NAME (sym, ty)          -> sprintf "%s -> %s" (Symbol.name sym) (match !ty with | None -> "?" | Some t -> string_of_ty t)
and string_of_field (symbol, ty) =
  sprintf "%s: %s" (Symbol.name symbol) (string_of_ty ty)

(* 查找真正的隐含类型，其中NAME在有值时返回一个除NAME外的类型*)
let rec actual_ty = function
  | INT                     -> INT
  | STRING                  -> STRING
  | RECORD (fields, unique) -> RECORD (List.map (fun (symbol, ty) -> (symbol, actual_ty ty)) fields, unique)
  | ARRAY (ty, unique)      -> ARRAY (actual_ty ty, unique)
  | NIL                     -> NIL
  | UNIT                    -> UNIT
  | NAME (symbol, ty)       ->
      match !ty with
      | None -> failwith (sprintf "type '%s' is not defined." (Symbol.name symbol))
      | Some (t) -> actual_ty t

(* 检查两个类型是否相等 *)
let check_ty expected actual =
  match (expected, actual) with
  | (INT, INT)                 -> true
  | (STRING, STRING)           -> true
  | (NIL, NIL)                 -> true
  | (UNIT, UNIT)               -> true
  | (RECORD (_, unique1), RECORD (_, unique2)) -> unique1 == unique2
  | (ARRAY (_, unique1), ARRAY (_, unique2))   -> unique1 == unique2
  | _ -> false
