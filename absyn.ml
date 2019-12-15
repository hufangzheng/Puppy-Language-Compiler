module L = Location
let symbol  = Symbol.symbol

module Absyn =
  struct
    type pos = pos = L.t
    type var = SimpleVar of symbol * pos
             | FieldVar of var * symbol * pos
             | SubscriptVar of var * exp * pos
    and exp =
      | VarExp of var
      | NilExp
      | IntExp of int
      | StringExp of string
      | CallExp of {func: symbol; args: exp list; pos: pos}
      | OpExp of {left: exp; op: op; right: exp}
      | RecordExpr {fields: (symbol * exp * pos); typ: symbol; pos: pos}
      | SeqExp of (exp * pos) list
      | AssignExp of {var: var; exp: exp; pos:pos}
      | IfExp of {test: exp; then':exp; else': exp option; pos: pos}
      | WhileExp of {test: exp; body: exp; pos: pos}
      | ForExp of {var: symbol; escape: bool ref; lo: exp; hi: exp; pos: pos}
      | BreakExp of pos
      | LetExp of {decs: dec list; body: exp; pos: pos}
      | ArrayExp of {typ: symbol; size: exp; init: exp; pos: pos}
    and dec =
      | FunctionDec of fundec list
      | VarDec of {name: symbol; escape: bool ref; typ: (symbol * pos) option; init: exp; pos: pos}
      | TypeDec of {name: symbol; ty: ty; pos: pos}
    and ty =
      | NameTy of symbol * pos
      | RecordTy of field list
      | ArrayTy of symbol * pos
    and op =
      | PlusOp | MinusOp | TimesOp | DivideOp | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp
    and tyfield = {name: symbol; escape: bool ref; typ: symbol; pos: pos}
    and funcdec = {name: symbol; params: field list; result: (symbol * pos) option; body: exp; pos: pos}

  end;;
