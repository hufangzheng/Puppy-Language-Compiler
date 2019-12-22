(* module L = Location *)
    type symbol  = Symbol.symbol
    type pos = Lexing.position
    type exp =
      | NilExp of pos
      | IntExp of int
      | LValue of {l: lvalue;pos: pos}
      | StringExp of string * pos
      | CallExp of {func: symbol; args: exp list; pos: pos}
      | OpExp of {left: exp; op: op; right: exp; pos: pos}
      | RecordExp of {rec_exp_fields: (symbol * exp * pos) list; typ: symbol; pos: pos}
      | SeqExp of (exp * pos) list
      | AssignExp of {lvalue: lvalue; exp: exp; pos:pos}
      | IfExp of {test: exp; then':exp; else': exp option; pos: pos}
      | WhileExp of {test: exp; body: exp; pos: pos}
      | ForExp of {var: symbol; escape: bool ref; lo: exp; hi: exp; body: exp; pos: pos}
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
      | PlusOp | MinusOp | TimesOp | DivideOp | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp | AndOp | OrOp

    and lvalue =
      | Ident of { name: symbol; pos: pos }
      | RecordAccess of {record: lvalue; name: symbol; pos: pos }
      | ArrayAccess of { array: lvalue; exp: exp; pos: pos }

    and field = {name: symbol; escape: bool ref; typ: symbol; pos: pos}

    and fundec = {name: symbol; params: field list; result: (symbol * pos) option; body: exp; pos: pos}
