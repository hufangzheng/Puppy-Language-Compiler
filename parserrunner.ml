open Absyn
open Printf

let string_of_pos pos =
  "(" ^ string_of_int (Lexer.line pos) ^ ", " ^ string_of_int (Lexer.col pos) ^ ")"

let rec string_of_exp = function
  | NilExp pos                 -> sprintf "Nil(%s)" (string_of_pos pos)
  | IntExp num                 -> sprintf "IntExp(%d)" num
  | LValue {l; pos}            -> sprintf "LValue(%s, %s)" (string_of_lvalue l) (string_of_pos pos)
  | StringExp (str, pos)       -> sprintf "StringExp(%s, %s)" str (string_of_pos pos)
  | CallExp {func; args; pos}  -> sprintf "CallExp(%s(%s), %s)" (Symbol.name func) (String.concat ", " (List.map string_of_exp args)) (string_of_pos pos)
  | OpExp {left; op; right; pos}    -> sprintf "OpExp(%s, %s)" (string_of_opexp left op right) (string_of_pos pos)
  | RecordExp {rec_exp_fields; typ; pos} -> sprintf "RecordExp(%s, {%s}, %s)" (Symbol.name typ) (String.concat "; " (List.map string_of_recordfield rec_exp_fields)) (string_of_pos pos)
  | SeqExp (explist)           -> sprintf "SeqExp([%s])" (String.concat "; " (List.map string_of_expseq explist))
  | AssignExp {lvalue; exp; pos} -> sprintf "AssignExp(%s := %s, %s)" (string_of_lvalue lvalue) (string_of_exp exp) (string_of_pos pos)
  | IfExp {test; then'; else'; pos} -> sprintf "IfExp(if %s, then %s, else %s, %s)"(string_of_exp test)
(string_of_exp then')
(match else' with
   | None            -> ""
   | Some exp        -> string_of_exp exp)
(string_of_pos pos)
  | WhileExp {test; body; pos} -> sprintf "WhileExp(test %s, body %s, %s)" (string_of_exp test) (string_of_exp body) (string_of_pos pos)
  | ForExp {var; escape; lo; hi; body; pos} -> sprintf "ForExp(var %s, escape %s, lo %s, hi %s, body %s, %s)" (Symbol.name var) (string_of_bool !escape) (string_of_exp lo) (string_of_exp hi) (string_of_exp body) (string_of_pos pos)
  | BreakExp pos               -> sprintf "BreakExp(%s)" (string_of_pos pos)
  | LetExp {decs; body; pos}   -> sprintf "LetExp(decs %s, body %s, %s)" 
(String.concat "\n" (List.map string_of_decl decs))
("\n" ^ (string_of_exp body))
(string_of_pos pos)
  | ArrayExp {typ; size; init; pos} -> sprintf "ArrayExp(typ %s, size %s, init %s, %s)" 
(Symbol.name typ)
(string_of_exp size)
(string_of_exp init)
(string_of_pos pos)
and string_of_decl = function
  | FunctionDec funlist        -> sprintf "FunctionDec([%s])" (String.concat "; " (List.map string_of_fundec funlist))
  | VarDec {name; escape; typ; init; pos} -> sprintf "VarDec(name %s, escape %s, typ %s, init %s, %s)"
(Symbol.name name)
(string_of_bool !escape)
(match typ with
   | None               -> ""
   | Some(symbol, pos)  -> (Symbol.name symbol) ^ ", " ^ (string_of_pos pos))
(string_of_exp init)
(string_of_pos pos)
  | TypeDec {name; ty; pos}    -> sprintf "TypeDec(name %s, ty %s, %s)" (Symbol.name name) (string_of_type ty) (string_of_pos pos)
and string_of_type = function
  | NameTy(symbol, pos)   -> sprintf "NameTy(symbol %s, %s)" (Symbol.name symbol) (string_of_pos pos)
  | RecordTy fieldlist    -> sprintf "RecordTy(field [%s])" (String.concat ", " (List.map string_of_field fieldlist))
  | ArrayTy(symbol, pos)  -> sprintf "ArrayTy(symbol %s, %s)" (Symbol.name symbol) (string_of_pos pos)
and string_of_fundec {name; params; result; body; pos} =
  sprintf "FunDec(name %s, params [%s], result %s, body %s, %s)" 
(Symbol.name name)
(String.concat ", " (List.map string_of_field params))
(match result with
   | None                 -> ""
   | Some(symbol, pos)   -> (Symbol.name symbol) ^ ", " ^ (string_of_pos pos))
(string_of_exp body)
(string_of_pos pos)
and string_of_field {name; escape; typ; pos} =
  sprintf "Field(name %s, escape %s, typ %s, %s)" 
(Symbol.name name)
(string_of_bool !escape)
(Symbol.name typ)
(string_of_pos pos)
and string_of_lvalue = function
  | Ident {name; pos}          -> sprintf "Ident(%s, %s)" (Symbol.name name) (string_of_pos pos)
  | RecordAccess {record; name; pos}  -> sprintf "RecordAccess(%s.%s, %s)" (string_of_lvalue record) (Symbol.name name) (string_of_pos pos)
  | ArrayAccess {array; exp; pos}     -> sprintf "ArrayAccess(%s[%s], %s)" (string_of_lvalue array) (string_of_exp exp) (string_of_pos pos)

and string_of_opexp l op r =
  match op with
  | PlusOp         -> string_of_binop l " + " r
  | MinusOp        -> string_of_binop l " - " r
  | TimesOp        -> string_of_binop l " * " r
  | DivideOp       -> string_of_binop l " / " r
  | EqOp           -> string_of_binop l " = " r
  | NeqOp          -> string_of_binop l " <> " r
  | LtOp           -> string_of_binop l " < " r
  | LeOp           -> string_of_binop l " <= " r
  | GtOp           -> string_of_binop l " > " r
  | GeOp           -> string_of_binop l " >= " r
  | AndOp          -> string_of_binop l " && " r
  | OrOp           -> string_of_binop l " || " r

and string_of_binop l op r =
  (string_of_exp) l ^ op ^ (string_of_exp r)

and string_of_recordfield (symbol, exp, pos) =
  (Symbol.name symbol) ^ " = " ^ (string_of_exp exp) ^ ", " ^ (string_of_pos pos)

and string_of_expseq (exp, pos) =
  (string_of_exp exp) ^ ", " ^ (string_of_pos pos)

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.prog Lexer.puppy lexbuf in
  print_endline (string_of_exp ast)
